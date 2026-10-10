"""Tests for the text and position helpers of the handler fallbacks.

These helpers used to be private functions of the Solidity fallback and are now
shared by every fallback (`core/handler/textpos.py').  They work on the text of a
file and on offsets into it, so they can be tested without a language server,
which is what this unittest does - it is also the check that splitting the file
did not change what they answer.
"""

import tempfile
import unittest
from pathlib import Path

from core.handler import textpos


class WordAt(unittest.TestCase):
    def test_extracts_the_word_around_the_column(self):
        self.assertEqual(textpos.extract_word_at("uint256 balance;", 10), "balance")

    def test_a_position_on_the_boundary_gives_the_word_before_it(self):
        # This is what the language server asks for at the end of a word.
        self.assertEqual(textpos.extract_word_at("uint256 balance;", 7), "uint256")

    def test_returns_none_when_there_is_no_word(self):
        self.assertIsNone(textpos.extract_word_at("   ", 1))

    def test_span_covers_the_whole_word(self):
        line = "uint256 balance;"
        span = textpos.extract_word_span_at(line, 10)
        self.assertEqual(line[span[0]:span[1]], "balance")


class Offsets(unittest.TestCase):
    def test_line_start_offsets_count_every_line(self):
        self.assertEqual(textpos.line_start_offsets("a\nbb\n\nccc"), [0, 2, 5, 6])

    def test_index_to_position_counts_from_zero(self):
        # LSP positions are zero based, and so are these.
        content = "a\nbb\nccc"
        self.assertEqual(textpos.index_to_position(content, 0), {"line": 0, "character": 0})
        self.assertEqual(textpos.index_to_position(content, 5), {"line": 2, "character": 0})


class MatchingParen(unittest.TestCase):
    def test_finds_the_matching_close(self):
        content = "f(a, g(b), c)"
        self.assertEqual(textpos.find_matching_paren(content, 1), 12)

    def test_ignores_parentheses_inside_strings(self):
        content = 'f(")", g(b))'
        self.assertEqual(textpos.find_matching_paren(content, 1), 11)

    def test_returns_none_when_unbalanced(self):
        self.assertIsNone(textpos.find_matching_paren("f(a", 1))


class TopLevelItems(unittest.TestCase):
    def test_counts_a_plain_list(self):
        self.assertEqual(textpos.count_top_level_items("a, b, c"), 3)

    def test_does_not_split_nested_commas(self):
        self.assertEqual(textpos.count_top_level_items("a, f(b, c), d"), 3)

    def test_counts_an_empty_text_as_nothing(self):
        self.assertEqual(textpos.count_top_level_items(""), 0)


class Reading(unittest.TestCase):
    def test_reads_a_file_and_reports_a_missing_one(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "sample.sol"
            path.write_text("contract A {}\n", encoding="utf-8")
            self.assertEqual(textpos.read_text_file(str(path)), "contract A {}\n")
            self.assertIsNone(textpos.read_text_file(str(path) + ".missing"))


if __name__ == "__main__":
    unittest.main()
