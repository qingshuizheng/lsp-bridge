"""Tests for the two decisions the bridge server makes without a language server.

Starting a server needs sockets, processes and Emacs; but two of the things the bridge does
before that are plain: which server names a multi-server configuration refers to, and which
directory is the root of a project.  Both are asked here, the second on real directories
made by the test.
"""

import os
import shutil
import tempfile
import unittest

from core.bridge_server import ServerBridgeMixin


def bridge():
    """Return a bridge mixin without running its constructor."""
    return ServerBridgeMixin.__new__(ServerBridgeMixin)


class PickMultiServerNamesTest(unittest.TestCase):
    """The names a multi-server configuration refers to."""

    def pick(self, info):
        """Return the names picked out of INFO."""
        return bridge().pick_multi_server_names(info)

    def test_a_single_name_is_taken_as_it_is(self):
        self.assertEqual(self.pick({"a": "one", "b": "two"}), ["one", "two"])

    def test_a_list_of_names_is_spliced_in(self):
        self.assertEqual(self.pick({"a": "one", "b": ["two", "three"]}), ["one", "two", "three"])

    def test_the_order_of_first_appearance_is_kept(self):
        self.assertEqual(self.pick({"a": ["two"], "b": "one", "c": ["two", "three"]}),
                         ["two", "one", "three"])

    def test_a_configuration_with_nothing_in_it(self):
        self.assertEqual(self.pick({}), [])


class FindProjectRootTest(unittest.TestCase):
    """The closest ancestor holding one of the marker files."""

    def setUp(self):
        self.root = tempfile.mkdtemp(prefix="lsp-bridge-project")
        self.addCleanup(shutil.rmtree, self.root, True)
        self.marker = os.path.join(self.root, "Cargo.toml")
        with open(self.marker, "w") as f:
            f.write("[package]\\n")
        self.deep = os.path.join(self.root, "one", "two", "three")
        os.makedirs(self.deep)

    def test_the_closest_ancestor_holding_the_marker(self):
        self.assertEqual(bridge().find_project_root(self.deep, ["Cargo.toml"]), self.root)
        # The root itself is where the marker is.
        self.assertEqual(bridge().find_project_root(self.root, ["Cargo.toml"]), self.root)

    def test_a_shallower_marker_is_not_preferred(self):
        # A second marker nearer than the first is what is returned.
        nearer = os.path.join(self.root, "one")
        with open(os.path.join(nearer, "package.json"), "w") as f:
            f.write("{}\\n")
        self.assertEqual(bridge().find_project_root(self.deep, ["package.json"]), nearer)

    def test_no_marker_at_all(self):
        self.assertIsNone(bridge().find_project_root(self.deep, ["NothingHere"]))

    def test_the_search_stops_at_the_depth_given(self):
        # The depth counts the directories inspected, the one the search starts in
        # among them: `three' is the first, `two' the second, `one' the third.
        self.assertIsNone(bridge().find_project_root(self.deep, ["Cargo.toml"], max_depth=2))
        self.assertIsNone(bridge().find_project_root(self.deep, ["Cargo.toml"], max_depth=3))
        self.assertEqual(bridge().find_project_root(self.deep, ["Cargo.toml"], max_depth=4),
                         self.root)


if __name__ == "__main__":
    unittest.main()
