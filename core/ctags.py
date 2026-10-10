#! /usr/bin/env python
# -*- coding: utf-8 -*-

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

import subprocess
import threading
import re
import os

from core.utils import *

# readtags filter that drops anonymous, reference and input-file tags.
DEFAULT_FILTER_CMD = '(not (or (and $extras ((string->regexp "(^|,) ?(anonymous|reference)(,|$)" :case-fold false) $extras)) (or (and $extras ((string->regexp "(^|,) ?(inputFile)(,|$)" :case-fold false) $extras)) (and $kind ((string->regexp "^(file|F)$" :case-fold false) $kind))) false))'

# readtags sorter: shorter names first, then alphabetically.
DEFAULT_SORTER_CMD = '(<or> (if (and $name &name) (<> (length $name) (length &name)) 0) (if (and $name &name) (<> $name &name) 0))'

# for Python 3.9-
def remove_prefix(text, prefix):
    """Return TEXT with PREFIX removed, or TEXT when it does not start with it.

    Kept for Python 3.9, which has no `str.removeprefix`.
    """
    if text.startswith(prefix):
        return text[len(prefix):]
    return text

class Ctags:    
    """Complete project symbols from the ctags `tags` file.

    Every query shells out to `readtags` in the directory of the file being
    edited: `make_complete` feeds the completion menu and `find_definition`
    answers jump-to-definition.  A result is dropped when the cursor has moved
    on, which is what the lock and `current_cursor_offset` guard.
    """
    def __init__(self) -> None:
        """Create the backend with no cursor offset recorded yet."""
        self.current_cursor_offset = 0
        self.lock = threading.RLock()

    def run_cmd_in_path(self, cmd, filename, in_shell=False):
        """Run CMD in the directory of FILENAME and return its output lines.

        A FILENAME that is not a file is treated as a directory.  IN_SHELL
        non-nil hands the command to the shell.  A non-zero exit status is
        logged and returns no lines; empty lines are dropped either way.
        """
        if os.path.isfile(filename):
            cwd = os.path.dirname(filename)
        else:
            cwd = filename

        result = subprocess.run(cmd, cwd=cwd, shell=in_shell, capture_output=True, text=True)

        # Get the output as a string
        output = ""
        status = result.returncode
        if status != 0:
            logger.error(result.stderr)
        else:
            output = result.stdout

        lines = output.split('\n')
        lines = [l for l in lines if l != '']

        return lines

    def parse_tag_line(self, line):
        """Parse one `tags` line into a dict of its fields.

        Always holds `tagname`, `tagfile` and `tagaddress`, plus every extension
        field the line carries (`kind`, `signature`, `scope`, ...).  An empty
        line yields None.
        """
        if line == '':
            return None

        tag = {}
        entry = line.rsplit(';"', 1)

        normal_field = entry[0].split('\t', 2)
        tag['tagname'] = normal_field[0]
        tag['tagfile'] = normal_field[1]
        tag['tagaddress'] = normal_field[2]

        if len(entry) == 2:
            for field in entry[1].split('\t')[1:]:
                field_entry = field.split(':', 1)
                if len(field_entry) == 2:
                    tag.update(dict([field_entry]))
                else:
                    tag.update({"kind": field_entry})

        return tag

    def make_tag_annotation(self, tag):
        """Build the annotation shown next to a ctags candidate.

        Combines the reference marker, kind, type and scope of TAG, as in
        `<R>function/typename@Class`; anonymous type names are shortened to
        `__anon`.
        """
        kind = tag.get("kind", "")
        typeref = tag.get("typeref", "")
        scope = tag.get("scope", "")
        extras = tag.get("extras", "")
        reference = "<R>" if "reference" in extras else ""

        typeref = remove_prefix(typeref, "typename:")
        typeref = re.sub(r'(^|:)(__anon[^:]+)(:|$)', r'\1__anon\3', typeref)
        scope = re.sub(r'(^|:)(__anon[^:]+)(:|$)', r'\1__anon\3', scope)

        annotation = (reference, 
                      kind, 
                      "/" if (kind != "" and typeref != "") else "",
                      typeref,
                      "@" if (scope != "") else "",
                      scope)

        return ''.join(annotation)

    def make_ctags_acm_candidate(self, tag: dict):
        """Turn the parsed TAG into a candidate for the completion menu."""
        candidate = {}
        tagname = tag["tagname"]

        kind = tag.get("kind", "NOKIND")
        signature = tag.get("signature", "NOSIGN")
        annotation = self.make_tag_annotation(tag)

        candidate["key"] = tagname
        candidate["icon"] = kind
        candidate["label"] = tagname
        candidate["displayLabel"] = tagname
        candidate["annotation"] = annotation
        candidate["backend"] = "ctags"

        return candidate

    def locate_dominating_file(self, file_or_dir, filename):
        """Look for FILENAME in FILE_OR_DIR and in its parent directories.

        Returns the first path that exists, or None when no ancestor contains
        it.  This is how the `tags` file governing a project is found.
        """
        current_dir = os.path.abspath(file_or_dir)
        
        while True:
            if os.path.exists(os.path.join(current_dir, filename)):
                return os.path.join(current_dir, filename)
            
            if current_dir == os.path.dirname(current_dir):
                break

            current_dir = os.path.dirname(current_dir)
            
        return None

    def make_complete(self, symbol, filename, max_lines, cursor_offset):
        """Complete SYMBOL from the tags file that governs FILENAME.

        Reports at most MAX_LINES candidates.  CURSOR_OFFSET records where the
        cursor was, so a result that arrives after the user moved on is
        discarded.  Does nothing without a filename or a tags file.
        """
        self.lock.acquire()
        try:
            self.current_cursor_offset = cursor_offset
        finally:
            self.lock.release()

        if not filename:
            return

        tagsfile = self.locate_dominating_file(filename, "tags")
        if not tagsfile:
            return
        
        cmd = self.readtags_get_cmd(tagsfile, symbol, "prefix", False, DEFAULT_FILTER_CMD, DEFAULT_SORTER_CMD, "")

        lines = self.run_cmd_in_path(cmd, filename)
        lines = lines[0:max_lines]
        
        tags = map(self.parse_tag_line, lines)
        candidates = map(self.make_ctags_acm_candidate, tags)

        self.dispatch(list(candidates), cursor_offset)

    def make_xref(self, tag: dict, rootdir):
        """Turn a parsed TAG into a jump-to-definition item.

        ROOTDIR is the directory holding the tags file; the item carries the
        absolute (possibly TRAMP-prefixed) path, the line number and the matched
        text.
        """
        candidate = {}
        tagname = tag["tagname"]

        path = tag.get("tagfile", "")
        line = int(tag.get("line", "1"))
        pattern = tag.get("tagaddress", "")
        annotation = self.make_tag_annotation(tag)

        ext_abspath = os.path.join(rootdir, path)
        tramp_method = get_remote_connection_info()

        candidate["ext-abspath"] = tramp_method + ext_abspath
        candidate["line"] = line
        candidate["str"] = pattern[2:-2]
        candidate["annotation"] = annotation

        return candidate

    def find_definition(self, symbol, filename):
        """Jump to the definition of SYMBOL using FILENAME's tags file.

        Exact matches are reported through `acm-backend-ctags-xref-callback`; if
        nothing matches, a message is printed instead.
        """
        tagsfile = self.locate_dominating_file(filename, "tags")
        if not tagsfile:
            return

        cmd = self.readtags_get_cmd(tagsfile, symbol, "exact", False, "", "", "")
        lines = self.run_cmd_in_path(cmd, filename)
        tags = map(self.parse_tag_line, lines)

        candidates = map(lambda tag: self.make_xref(tag, os.path.dirname(tagsfile)), tags)
        candidates = list(candidates)

        if candidates == []:
            message_emacs(f"symbol {symbol} not found by ctags")
        else:
            eval_in_emacs("acm-backend-ctags-xref-callback", candidates)

    def dispatch(self, candidates, cursor_offset):
        """Report CANDIDATES to Emacs while the cursor is at CURSOR_OFFSET."""
        self.lock.acquire()
        try:
            if self.current_cursor_offset == cursor_offset:
                eval_in_emacs("lsp-bridge-search-backend--record-items", "ctags", candidates)
        finally:
            self.lock.release()

    def readtags_get_cmd(self, tagsfile, name, match, case_fold, filter, sorter, action):
        """Build the `readtags` command line of a query.

        TAGSFILE is the tags file to read, NAME the symbol to look up, MATCH is
        "exact" or "prefix" and CASE_FOLD selects case-insensitive matching.
        FILTER, SORTER and ACTION are raw readtags expressions; an empty ACTION
        lists the matching tags.
        """
        extras = ("-Ene" +
                  ("" if match == "exact" else "p") +
                  ("i" if case_fold else ""))
        cmd = []

        # program name        
        cmd.append("readtags")

        # read from this tags file
        cmd.append("-t")
        cmd.append(tagsfile)

        # filter expresion
        if filter:
            cmd.append("-Q")
            cmd.append(filter)
        if sorter:
            cmd.append("-S")
            cmd.append(sorter)

        # extras arguments
        cmd.append(extras)

        # action
        if action:
            cmd.append(action)
        else:
            if not name:
                cmd.append("-l")
            else:
                cmd.append("-")
                cmd.append(name)

        return cmd
