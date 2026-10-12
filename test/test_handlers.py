"""Tests for the logic the handlers own, without a language server.

A handler is mostly a request and a response, and the part of it that can be wrong
without a server is what it makes of the two: the workspace edit a rename answers
with, for instance, has two shapes and a count that is reported to the user.  What
is checked here is that kind of logic, so it runs anywhere and fast.
"""

import importlib
import inspect
import pkgutil
import unittest

from core.handler import Handler
from core.handler.code_action import CodeAction
from core.handler.completion import Completion
from core.handler.completion_workspace_symbol import CompletionWorkspaceSymbol
from core.handler.document_highlight import DocumentHighlight
from core.handler.execute_command import ExecuteCommand
from core.handler.hover import Hover, make_code_block
from core.handler.prepare_rename import PrepareRename
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


class FixEmptyObjectsTest(unittest.TestCase):
    """clojure-lsp is sent `null' instead of an empty object, everywhere."""

    def setUp(self):
        self.execute = bare(ExecuteCommand)

    def test_empty_object_becomes_none(self):
        self.assertIsNone(self.execute.fix_empty_objects({}))

    def test_nested_objects_and_lists(self):
        self.assertEqual(
            self.execute.fix_empty_objects({"a": {}, "b": [{}], "c": {"d": {}}}),
            {"a": None, "b": [None], "c": {"d": None}})

    def test_other_values_are_untouched(self):
        self.assertEqual(self.execute.fix_empty_objects([1, "two", None, []]),
                         [1, "two", None, []])


class CompareWorkspaceSymbolsTest(unittest.TestCase):
    """The order the workspace symbols are shown in."""

    def setUp(self):
        self.symbols = bare(CompletionWorkspaceSymbol)
        self.symbols.prefix = "foo"

    def test_prefix_matches_come_first(self):
        match = {"label": "foobar"}
        other = {"label": "barfoo"}
        self.assertEqual(self.symbols.compare_candidates(match, other), -1)
        self.assertEqual(self.symbols.compare_candidates(other, match), 1)

    def test_the_shorter_label_comes_last(self):
        short = {"label": "foo"}
        long = {"label": "foobar"}
        self.assertEqual(self.symbols.compare_candidates(short, long), -1)
        self.assertEqual(self.symbols.compare_candidates(long, short), 1)

    def test_equal_labels_are_equal(self):
        self.assertEqual(self.symbols.compare_candidates({"label": "foo"},
                                                         {"label": "FOO"}), 0)


class MakeCodeBlockTest(unittest.TestCase):
    """Documentation that is not Markdown already is fenced."""

    def test_a_fenced_block(self):
        self.assertEqual(make_code_block("python", "print(1)"),
                         "```python\nprint(1)\n```")


class ParseHoverContentsTest(unittest.TestCase):
    """The shapes `contents' arrives in are normalised into Markdown."""

    def setUp(self):
        self.hover = bare(Hover)

    @staticmethod
    def parse(contents):
        strings = []
        return Hover.parse_hover_contents(bare(Hover), contents, strings)

    def test_plain_text_is_fenced(self):
        self.assertEqual(self.parse("some text"), "```text\nsome text\n```")

    def test_markdown_is_kept_as_it_is(self):
        self.assertEqual(self.parse("```python\nprint(1)\n```"),
                         "```python\nprint(1)\n```")

    def test_a_markdown_content_object_is_appended(self):
        self.assertEqual(self.parse({"kind": "markdown", "value": "# Title"}),
                         "# Title")

    def test_a_language_object_is_fenced(self):
        self.assertEqual(self.parse({"language": "python", "value": "print(1)"}),
                         "```python\nprint(1)\n```")

    def test_nothing_to_render(self):
        self.assertEqual(self.parse([]), "")


def collected_handlers():
    """Return the concrete handler classes under core/handler, by class name.

    Importing every module is part of the check: a handler that cannot be
    imported is a handler the bridge cannot use.  A class that only exists to be
    inherited from - `CallHierarchy' needs a method its two subclasses supply -
    is left out, since the bridge instantiates the concrete ones."""
    import core.handler
    found = {}
    for info in pkgutil.walk_packages(core.handler.__path__, "core.handler."):
        module = importlib.import_module(info.name)
        for name, obj in inspect.getmembers(module, inspect.isclass):
            if (obj is not Handler and issubclass(obj, Handler)
                    and obj.__module__ == module.__name__):
                found[name] = obj
    bases = {base for handler in found.values() for base in handler.__mro__[1:]
             if base in found.values()}
    return {name: handler for name, handler in found.items() if handler not in bases}


class HandlerInterfaceTest(unittest.TestCase):
    """The shape every handler has to have, which the bridge relies on."""

    @classmethod
    def setUpClass(cls):
        cls.handlers = collected_handlers()

    def test_there_are_handlers_to_check(self):
        self.assertGreater(len(self.handlers), 20)

    def test_every_handler_declares_a_name_and_a_method(self):
        for name, handler in self.handlers.items():
            self.assertIsInstance(getattr(handler, "name", None), str, name)
            self.assertIsInstance(getattr(handler, "method", None), str, name)

    def test_handler_names_are_unique(self):
        names = [handler.name for handler in self.handlers.values()]
        self.assertEqual(sorted(names), sorted(set(names)))

    def test_every_handler_answers_both_ways(self):
        for name, handler in self.handlers.items():
            self.assertTrue(callable(getattr(handler, "process_request", None)), name)
            self.assertTrue(callable(getattr(handler, "process_response", None)), name)

    def test_every_handler_documents_itself(self):
        for name, handler in self.handlers.items():
            self.assertTrue(inspect.getdoc(handler), name)


class PrepareRenameTest(unittest.TestCase):
    """The placeholder a rename would replace."""

    def test_the_position_is_sent_as_it_is(self):
        position = {"line": 3, "character": 7}
        self.assertEqual(PrepareRename.process_request(bare(PrepareRename), position),
                         {"position": position})


class DocumentHighlightTest(unittest.TestCase):
    """The occurrences of the symbol at the cursor."""

    def test_the_position_is_sent_as_it_is(self):
        position = {"line": 0, "character": 1}
        self.assertEqual(
            DocumentHighlight.process_request(bare(DocumentHighlight), position),
            {"position": position})

    def test_a_change_cancels_the_request(self):
        self.assertTrue(DocumentHighlight.cancel_on_change)


class CodeActionTest(unittest.TestCase):
    """The actions a server offers for a range, and the filter it asks for."""

    def setUp(self):
        self.handler = bare(CodeAction)

    def test_a_string_kind_is_sent_as_the_only_filter(self):
        params = self.handler.process_request("pyright", [{"code": 1}],
                                              {"line": 1}, {"line": 2}, "quickfix")
        self.assertEqual(params, {"range": {"start": {"line": 1}, "end": {"line": 2}},
                                  "context": {"diagnostics": [{"code": 1}],
                                              "only": ["quickfix"]}})
        self.assertEqual(self.handler.action_kind, "quickfix")
        self.assertEqual(self.handler.lsp_server_name, "pyright")

    def test_a_kind_that_is_not_a_string_leaves_the_filter_out(self):
        params = self.handler.process_request("clangd", [], {"line": 0}, {"line": 0}, None)
        self.assertEqual(params["context"], {"diagnostics": []})
        self.assertIsNone(self.handler.action_kind)


if __name__ == "__main__":
    unittest.main()
