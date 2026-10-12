"""Tests for the logic the handlers own, without a language server.

A handler is mostly a request and a response, and the part of it that can be wrong
without a server is what it makes of the two: the workspace edit a rename answers
with, for instance, has two shapes and a count that is reported to the user.  What
is checked here is that kind of logic, so it runs anywhere and fast.
"""

import unittest

from core.handler.completion import Completion
from core.handler.rename import count_workspace_edits
from core.handler.semantic_tokens import SemanticTokens


def bare(handler_class):
    """Return an instance of HANDLER_CLASS without the bridge behind it.

    The helpers checked here do not touch what the bridge gives a handler, so the
    instance is made without running the constructor."""
    return object.__new__(handler_class)


class CountWorkspaceEditsTest(unittest.TestCase):
    """The rename answer is counted in both of the shapes it can arrive in."""

    def test_document_changes_shape(self):
        response = {"documentChanges": [{"edits": [1, 2]}, {"edits": [3]}]}
        self.assertEqual(count_workspace_edits(response), 3)

    def test_changes_shape(self):
        response = {"changes": {"file://a": [1], "file://b": [1, 2]}}
        self.assertEqual(count_workspace_edits(response), 3)

    def test_empty_answer(self):
        self.assertEqual(count_workspace_edits({}), 0)
        self.assertEqual(count_workspace_edits({"changes": {}}), 0)


class FindOnesTest(unittest.TestCase):
    """The modifier bit set of a token is turned into face indices."""

    def setUp(self):
        self.tokens = bare(SemanticTokens)

    def test_set_bits(self):
        self.assertEqual(self.tokens.find_ones(0b1011), [0, 1, 3])

    def test_no_bits(self):
        self.assertEqual(self.tokens.find_ones(0), [])

    def test_high_bit(self):
        self.assertEqual(self.tokens.find_ones(1 << 20), [20])


class AbsoluteLineToRelativeTest(unittest.TestCase):
    """Tokens are re-encoded the delta way Emacs applies them."""

    def setUp(self):
        self.tokens = bare(SemanticTokens)

    def test_deltas_within_and_across_lines(self):
        absolute = [(0, 0, 1, 0, 0), (0, 2, 1, 0, 0), (2, 1, 2, 0, 0)]
        self.assertEqual(
            self.tokens.absolute_line_to_relative(absolute),
            [(0, 0, 1, 0, 0), (0, 2, 1, 0, 0), (2, 1, 2, 0, 0)])

    def test_columns_restart_on_a_new_line(self):
        absolute = [(4, 3, 1, 0, 0), (5, 2, 1, 0, 0)]
        self.assertEqual(
            self.tokens.absolute_line_to_relative(absolute),
            [(4, 3, 1, 0, 0), (1, 2, 1, 0, 0)])

    def test_no_tokens(self):
        self.assertEqual(self.tokens.absolute_line_to_relative([]), [])


class CalcDiffTokensTest(unittest.TestCase):
    """Only the runs that changed are rendered again."""

    def setUp(self):
        self.tokens = bare(SemanticTokens)

    def test_common_runs_are_dropped(self):
        before = {(0, 0, 1, 0, 0), (1, 0, 2, 0, 0)}
        now = {(1, 0, 2, 0, 0), (2, 0, 3, 0, 0)}
        self.assertEqual(self.tokens.calc_diff_tokens(before, now),
                         ([(2, 0, 3, 0, 0)], [(0, 0, 1, 0, 0)]))

    def test_nothing_changed(self):
        same = {(1, 0, 2, 0, 0)}
        self.assertEqual(self.tokens.calc_diff_tokens(same, same), ([], []))

    def test_the_new_runs_are_sorted_by_line(self):
        before = set()
        now = {(9, 0, 1, 0, 0), (3, 0, 1, 0, 0), (5, 0, 1, 0, 0)}
        new_tokens, old_tokens = self.tokens.calc_diff_tokens(before, now)
        self.assertEqual([token[0] for token in new_tokens], [3, 5, 9])
        self.assertEqual(old_tokens, [])


class ConvertSnippetTest(unittest.TestCase):
    """A repeated placeholder becomes a bare mirror, as YAS wants it."""

    def setUp(self):
        self.completion = bare(Completion)

    def test_a_repeated_placeholder_loses_its_default(self):
        self.assertEqual(
            self.completion.convert_snippet("${1:foo} then ${1:foo}"),
            "${1:foo} then ${1}")

    def test_placeholders_are_matched_by_their_default_text(self):
        # The implementation keys the mirrors on the default text, which is what
        # the YAS snippet syntax can express.
        self.assertEqual(
            self.completion.convert_snippet("${1:foo} ${2:foo} ${3:bar}"),
            "${1:foo} ${1} ${3:bar}")

    def test_a_snippet_without_placeholders_is_untouched(self):
        self.assertEqual(self.completion.convert_snippet("plain text"),
                         "plain text")


class Fnv1aTest(unittest.TestCase):
    """The auto-import cache is keyed by this hash, so it has to be stable."""

    def setUp(self):
        self.completion = bare(Completion)

    def test_same_bytes_same_hash(self):
        self.assertEqual(self.completion.fnv_1a(b"value"),
                         self.completion.fnv_1a(b"value"))

    def test_different_bytes_differ(self):
        self.assertNotEqual(self.completion.fnv_1a(b"value"),
                            self.completion.fnv_1a(b"valuf"))

    def test_every_byte_counts(self):
        self.assertNotEqual(self.completion.fnv_1a(b"\x00"),
                            self.completion.fnv_1a(b"\x01"))


class CompareCandidatesTest(unittest.TestCase):
    """The order the menu shows candidates in."""

    def setUp(self):
        self.completion = bare(Completion)
        self.completion.prefix = "foo"

    @staticmethod
    def candidate(label, score=1, sort_text=None, icon="function"):
        return {"label": label, "filterText": label, "score": score,
                "sortText": sort_text if sort_text is not None else label,
                "icon": icon}

    def test_prefix_matches_come_first(self):
        match = self.candidate("foobar")
        other = self.candidate("barfoo")
        self.assertEqual(self.completion.compare_candidates(match, other), -1)
        self.assertEqual(self.completion.compare_candidates(other, match), 1)

    def test_a_higher_score_comes_first(self):
        high = self.candidate("foo", score=10)
        low = self.candidate("foo", score=1)
        self.assertEqual(self.completion.compare_candidates(high, low), -1)

    def test_sort_text_breaks_a_tie(self):
        first = self.candidate("foo", sort_text="A0")
        second = self.candidate("foo", sort_text="B0")
        self.assertEqual(self.completion.compare_candidates(first, second), -1)

    def test_the_shorter_label_comes_last_in_a_tie(self):
        short = self.candidate("foo", sort_text="A")
        long = self.candidate("foobar", sort_text="A")
        self.assertEqual(self.completion.compare_candidates(short, long), -1)


if __name__ == "__main__":
    unittest.main()
