"""Tests for the choices a file action makes about servers and diagnostics.

A file action stands for one file of the editor and the servers that serve it.  The
part of it that is asked here is the part that is about data it holds - which servers
a method goes to, whether the file has one server or many, and how diagnostics are
counted and ordered - since the rest of the class talks to Emacs and to the servers
on the other side of the bridge.
"""

import unittest
from unittest import mock

from core.fileaction import FileAction


class StubFileAction(FileAction):
    """A file action that holds the attributes given and never talks to Emacs.

    It inherits the real methods, so a method that calls another one - as
    `get_lsp_server_names' calls `get_lsp_servers' - reaches the real code rather
    than a mock; only the attributes the tests set are their own.
    """

    def __init__(self, **attributes):
        """Take the attributes the test wants this action to hold."""
        for name, value in attributes.items():
            setattr(self, name, value)


def fake_action(**attributes):
    """Return a file action holding these attributes."""
    return StubFileAction(**attributes)


def diagnostic(start_line, start_character, end_line, end_character):
    """Return a diagnostic at the position given."""
    return {"range": {"start": {"line": start_line, "character": start_character},
                      "end": {"line": end_line, "character": end_character}}}


class SortDiagnosticTest(unittest.TestCase):
    """The order the diagnostics of one server are kept in."""

    def sort(self, diagnostic_a, diagnostic_b):
        """Return the comparison of two diagnostics."""
        return FileAction.sort_diagnostic(mock.Mock(), diagnostic_a, diagnostic_b)

    def test_earlier_lines_come_first(self):
        self.assertEqual(self.sort(diagnostic(1, 0, 1, 5), diagnostic(2, 0, 2, 5)), -1)
        self.assertEqual(self.sort(diagnostic(2, 0, 2, 5), diagnostic(1, 0, 1, 5)), 1)

    def test_the_character_decides_inside_a_line(self):
        self.assertEqual(self.sort(diagnostic(1, 0, 1, 5), diagnostic(1, 2, 1, 5)), -1)

    def test_the_end_position_breaks_a_tie(self):
        self.assertEqual(self.sort(diagnostic(1, 0, 1, 2), diagnostic(1, 0, 1, 5)), -1)

    def test_the_same_position_compares_equal(self):
        self.assertEqual(self.sort(diagnostic(1, 2, 3, 4), diagnostic(1, 2, 3, 4)), 0)


class ServerChoiceTest(unittest.TestCase):
    """Which server, or servers, a method of a file goes to."""

    def test_a_file_with_one_server_has_that_one(self):
        only = mock.Mock()
        action = fake_action(multi_servers={}, single_server=only)
        self.assertEqual(FileAction.get_lsp_servers(action), [only])
        self.assertEqual(FileAction.get_match_lsp_servers(action, "completion"), [only])

    def test_a_file_with_many_servers_has_their_values(self):
        first, second = mock.Mock(), mock.Mock()
        action = fake_action(multi_servers={"a": first, "b": second}, single_server=None)
        self.assertEqual(sorted(map(id, FileAction.get_lsp_servers(action))), sorted(map(id, [first, second])))

    def test_the_names_come_from_the_server_info(self):
        first, second = mock.Mock(), mock.Mock()
        first.server_info = {"name": "first"}
        second.server_info = {"name": "second"}
        action = fake_action(multi_servers={"first": first, "second": second})
        self.assertEqual(sorted(FileAction.get_lsp_server_names(action)), ["first", "second"])

    def test_a_method_may_name_a_list_of_servers(self):
        action = fake_action(multi_servers_info={"default": "d",
                                                 "completion": ["a", "b"],
                                                 "formatting": "c"})
        self.assertEqual(FileAction.get_method_server_names(action, "completion"), ["a", "b"])
        # Every other method is read as one name, wrapped in a list.
        self.assertEqual(FileAction.get_method_server_names(action, "formatting"), ["c"])
        # And one that is not configured falls back to the default.
        self.assertEqual(FileAction.get_method_server_names(action, "hover"), ["d"])

    def test_the_method_names_choose_the_servers(self):
        first, second = mock.Mock(), mock.Mock()
        action = fake_action(multi_servers={"a": first, "b": second},
                             multi_servers_info={"default": "a", "code_action": ["b"]})
        self.assertEqual(FileAction.get_match_lsp_servers(action, "code_action"), [second])


class DiagnosticBookkeepingTest(unittest.TestCase):
    """What the file action holds for the diagnostics it has been sent."""

    def test_the_count_is_the_sum_of_every_server(self):
        action = fake_action(diagnostics={"one": [1, 2], "two": [3]})
        self.assertEqual(FileAction.get_diagnostics_count(action), 3)

    def test_a_file_without_diagnostics_counts_nought(self):
        action = fake_action(diagnostics={})
        self.assertEqual(FileAction.get_diagnostics_count(action), 0)


if __name__ == "__main__":
    unittest.main()
