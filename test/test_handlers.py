"""Tests for the logic the handlers own, without a language server.

A handler is mostly a request and a response, and the part of it that can be wrong
without a server is what it makes of the two: the workspace edit a rename answers
with, for instance, has two shapes and a count that is reported to the user.  What
is checked here is that kind of logic, so it runs anywhere and fast.
"""

import unittest

from core.handler.rename import count_workspace_edits


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


if __name__ == "__main__":
    unittest.main()
