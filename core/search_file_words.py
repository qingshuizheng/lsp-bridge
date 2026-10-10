#! /usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2022 Andy Stewart
#
# Author:     Andy Stewart <lazycat.manatee@gmail.com>
# Maintainer: <lazycat.manatee@gmail.com> <lazycat.manatee@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import copy
import queue
import threading
import traceback
import re

from core.utils import *

class SearchFileWords:
    """Complete words taken from the files of the project.

    Emacs drives the index (`index_files`, `index_file`, `change_buffer`,
    `close_file`): a dispatcher thread re-reads the pending files, keeps their
    word sets in `files`, and `search` matches the prefix at point against the
    union of those sets.
    """
    def __init__(self):
        """Read the Emacs options and start the indexing dispatcher thread.

        `max_number`, `fuzzy_match` and `fuzzy_match_threshold` are read once
        here, so changing the corresponding Emacs options needs a restart.
        """
        self.files = {}
        self.search_files = set()
        self.search_content_dict = {}
        self.search_words_thread = None
        (self.max_number, self.fuzzy_match, self.fuzzy_match_threshold) = get_emacs_vars(["acm-backend-search-file-words-max-number", "acm-backend-search-file-words-enable-fuzzy-match", "acm-backend-search-file-words-enable-fuzzy-match-threshold"])
        self.search_words_queue = queue.Queue()
        self.search_words_dispatcher_thread = threading.Thread(target=self.search_dispatcher)
        self.search_words_dispatcher_thread.start()

    def index_files(self, filepaths):
        """Queue FILEPATHS for indexing.

        Files that are not indexed yet are registered and the dispatcher is
        woken up; files that are already known are not read again.
        """
        for filepath in filepaths:
            self.search_files.add(filepath)

        add_queue = False
        for filepath in filepaths:
            if filepath not in self.files:
                self.files[filepath] = set()
                add_queue = True

        if add_queue:
            self.search_words_queue.put("search_words")

    def index_file(self, filepath, content):
        """Index CONTENT as the current text of FILEPATH.

        The text is remembered in `search_content_dict` (so that later edits can
        be applied as diffs) and the dispatcher is asked to re-extract the
        words.
        """
        self.search_files.add(filepath)
        self.search_content_dict[filepath] = content

        if filepath not in self.files:
            self.files[filepath] = set()

        self.search_words_queue.put("search_words")

    def load_file(self, filepath, from_file_server=False):
        """Read FILEPATH and index it.

        FROM_FILE_SERVER non-nil fetches the content from the lsp-bridge file
        server instead of the local filesystem, which is how remote files are
        handled.  Read errors are swallowed.
        """
        try:
            if from_file_server:
                content = get_file_content_from_file_server(filepath)
            else:
                with open(filepath) as f:
                    content = f.read()
        except Exception:
            return

        self.index_file(filepath, content)

    def change_buffer(self, buffer_name, start_pos, end_pos, change_text):
        """Apply one buffer change to the indexed content of BUFFER_NAME.

        START_POS and END_POS are EPC argument lists describing the replaced
        range and CHANGE_TEXT is the replacement.  When nothing is known about
        the buffer yet, the whole content is fetched from Emacs instead.
        """
        start_pos = epc_arg_transformer(start_pos)
        end_pos = epc_arg_transformer(end_pos)

        if len(start_pos) == 0 or len(end_pos) == 0:
            return

        if buffer_name in self.search_content_dict:
            content = rebuild_content_from_diff(self.search_content_dict[buffer_name], start_pos, end_pos, change_text)
        else:
            content = get_emacs_func_result('get-buffer-content', buffer_name, True)

        self.index_file(buffer_name, content)

    def close_file(self, filepath):
        """Stop indexing FILEPATH and forget its words and content."""
        if filepath in self.files:
            if filepath in self.search_files:
                self.search_files.remove(filepath)

            if filepath in self.search_content_dict:
                del self.search_content_dict[filepath]

            del self.files[filepath]

    def search(self, prefix: str):
        """Start a threaded search for PREFIX over all indexed words."""
        self.search_words_thread = threading.Thread(target=lambda : self.search_words_from_files(prefix))
        self.search_words_thread.start()

    def search_words_from_files(self, prefix: str):
        """Search every indexed word for PREFIX and report the candidates.

        Runs in a worker thread.  When a plain search finds nothing and PREFIX
        looks like an elisp symbol, its last `-` or `_` segment is searched
        again and the remaining part of PREFIX is put back in front of each
        match.  Errors are logged, not raised.
        """
        try:
            all_words = set()
            for file, words in self.files.items():
                all_words = all_words | words

            search_candidates = self.search_word(prefix, all_words)
            candidates = []
            if len(search_candidates) > 0:
                candidates = search_candidates
            elif ("-" in prefix) and (not prefix.endswith("-")):
                search_prefix = prefix.split("-")[-1]
                candidates = self.search_word(search_prefix, all_words)

                candidates = list(map(lambda word: prefix[:-len(search_prefix)] + word, candidates))
            elif ("_" in prefix and (not prefix.endswith("_"))):
                search_prefix = prefix.split("_")[-1]
                candidates = self.search_word(search_prefix, all_words)

                candidates = list(map(lambda word: prefix[:-len(search_prefix)] + word, candidates))

            eval_in_emacs("lsp-bridge-search-backend--record-items", "search-file-words",
                          list(map(lambda word: {
                              "key": word,
                              "icon": "search",
                              "label": word,
                              "displayLabel": word,
                              "annotation": "Search Word",
                              "backend": "search-file-words"
                          }, candidates[:min(self.max_number, len(candidates))])))
        except Exception:
            logger.error(traceback.format_exc())

    def search_word(self, prefix, all_words):
        """Return the words of ALL_WORDS that match PREFIX.

        With the fuzzy option on, words are ranked with `rapidfuzz` and cut off
        at `fuzzy_match_threshold`; otherwise only words starting with PREFIX
        are kept, upper-cased when PREFIX is, and sorted shortest first.
        """
        candidates = []
        if self.fuzzy_match:
            from rapidfuzz import fuzz
            match_words = list(map(lambda word: {'word':word, 'ratio':fuzz.ratio(prefix.lower(), word.lower())}, all_words))
            match_words.sort(key=lambda x:x['ratio'], reverse=True)
            for word in match_words:
                if word['ratio'] >= self.fuzzy_match_threshold:
                    candidates.append(word['word'])
                else:
                    break
        else:
            match_words = list(filter(lambda word: word.lower().startswith(prefix.lower()), all_words))
            if prefix.isupper():
                candidates = list(map(lambda word: word.upper(), match_words))
            else:
                candidates = list(map(lambda word: prefix + word[len(prefix):], match_words))
            candidates.sort(key=len, reverse=False)
        return candidates

    def search_dispatcher(self):
        """Re-extract the words of every file waiting on `search_words_queue`.

        Runs until the process exits.  It snapshots the files that still need
        indexing, reads the ones whose content is not in memory, keeps the words
        that pass `filter_word`, and clears the pending set.  Errors are logged,
        not raised.
        """
        try:
            while True:
                message = self.search_words_queue.get(block=True)
                if message == "search_words":
                    search_files = copy.deepcopy(self.search_files)
                    search_content_dict = copy.deepcopy(self.search_content_dict)

                    for search_file in search_files:
                        try:
                            if search_file in search_content_dict:
                                words = set(re.findall(r"[\w|-]+", search_content_dict[search_file]))
                            else:
                                words = set(re.findall(r"[\w|-]+", open(search_file).read()))
                        except (FileNotFoundError, UnicodeDecodeError):
                            continue
                        filter_words = set(map(lambda word: re.sub(r'[^A-Za-z0-9-_]+', '', word),
                                               set(filter(self.filter_word, words))))
                        filter_words.discard("")
                        self.files[search_file] = filter_words

                    self.search_files.clear()
        except Exception:
            logger.error(traceback.format_exc())

    def filter_word(self, word: str):
        """Return True when WORD is worth indexing.

        Words of three characters or fewer, and purely numeric words, are
        skipped.
        """
        return len(word) > 3 and (not word.isnumeric())
