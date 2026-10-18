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

import functools
import threading
import re

from core.utils import eval_in_emacs


class SearchList:
    """Complete from word lists that Emacs hands over.

    Emacs owns the master lists (project words, dictionary words, ...) and calls
    `update` whenever one changes; `search` then matches the list for the prefix
    at point and pushes the result back as candidates.  Every backend name keeps
    its own symbols, size limit and ticker in `backend_dict`.
    """

    def __init__(self) -> None:
        """Create an empty registry of backend symbol lists."""
        self.backend_dict = {}
        self.search_thread_queue = []

    def update(self, backend_name, symbols, max_number):
        """Replace the symbol list of BACKEND_NAME with SYMBOLS.

        SYMBOLS is sorted by length so that the shortest candidates come first.
        MAX_NUMBER caps how many candidates a later search reports, but it is
        only stored the first time a backend is seen: updating a known backend
        keeps the original limit.  The ticker is left alone, so a search that is
        already running is not disturbed.
        """
        symbols = sorted(list(map(str, symbols)), key=len)

        if backend_name in self.backend_dict:
            self.backend_dict[backend_name]["symbols"] = symbols
        else:
            self.backend_dict[backend_name] = {
                "symbols": symbols,
                "max_number": max_number,
                "ticker": 0
            }

    def search(self, backend_name, prefix: str):
        """Search the list of BACKEND_NAME for PREFIX in a worker thread.

        A backend that was never registered with `update` is reported on stdout
        and otherwise ignored.
        """
        if backend_name in self.backend_dict:
            ticker = self.backend_dict[backend_name]["ticker"] + 1
            self.backend_dict[backend_name]["ticker"] = ticker

            search_thread = threading.Thread(target=lambda: self.search_symbols(backend_name, prefix, ticker))
            search_thread.start()
            self.search_thread_queue.append(search_thread)
        else:
            print("search_list error: {} not exist in backend_dict.".format(backend_name))

    def search_symbols(self, backend_name, prefix: str, ticker: int):
        """Collect and report the candidates of BACKEND_NAME for PREFIX.

        Runs in a worker thread: symbols are matched against a relaxed regexp
        built from PREFIX, at most `max_number` of them are kept, and the result
        is reported only when TICKER is still the current one.
        """
        candidates = []
        prefix_regexp = re.compile(re.sub(r'([a-zA-Z0-9-_])', r'\1.*', re.escape(prefix)))
        for symbol in self.backend_dict[backend_name]["symbols"]:
            if self.match_symbol(prefix, prefix_regexp, symbol):
                candidates.append(symbol)

                if len(candidates) > self.backend_dict[backend_name]["max_number"]:
                    break

        if ticker == self.backend_dict[backend_name]["ticker"]:
            eval_in_emacs("lsp-bridge-search-backend--record-items",
                          backend_name,
                          sorted(candidates, key=functools.cmp_to_key(lambda a, b: self.sort_symbols(prefix, a, b))))

    def match_symbol(self, prefix, prefix_regexp, symbol):
        """Return True when SYMBOL should be offered for PREFIX.

        A candidate matches when it starts with PREFIX, when it starts with
        PREFIX after its dashes are removed, or when it matches PREFIX_REGEXP
        (PREFIX with `.*` between its characters).
        """
        return symbol.startswith(prefix) or symbol.replace("-", "").startswith(prefix) or prefix_regexp.match(symbol)

    def sort_symbols(self, prefix, symbol_a, symbol_b):
        """Order two symbols, preferring the ones that start with PREFIX.

        Returns a bool (`len(symbol_a) < len(symbol_b)`) rather than the
        negative/zero/positive value a comparison usually returns; that is what
        the `functools.cmp_to_key` call in `search_symbols` consumes, so shorter
        symbols sort first.
        """
        symbol_a_starts_with_prefix = symbol_a.startswith(prefix)
        symbol_b_starts_with_prefix = symbol_b.startswith(prefix)

        if symbol_a_starts_with_prefix and symbol_b_starts_with_prefix:
            return len(symbol_a) < len(symbol_b)
        elif symbol_a_starts_with_prefix:
            return -1
        elif symbol_b_starts_with_prefix:
            return 1
        else:
            return len(symbol_a) < len(symbol_b)
