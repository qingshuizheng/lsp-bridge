"""Tests for the shape of the bridge process after the mixin split.

`LspBridge' is the object the Emacs side reaches over EPC, so splitting it has to
keep two things true: the class still carries every method the Emacs side calls,
and the three mixins it is built from do not overlap.  Both are checked here
without a language server, because they are about the shape of the class rather
than about what it does.
"""

import ast
import unittest
from pathlib import Path

import lsp_bridge
from core.bridge_remote import RemoteBridgeMixin
from core.bridge_search import SearchBridgeMixin
from core.bridge_server import ServerBridgeMixin

BRIDGE = Path(__file__).resolve().parent.parent / "lsp_bridge.py"

# The methods the Emacs side reaches over EPC in every session; the per file and
# per search backend entries are built at runtime and are not listed here.
EPC_ENTRY_POINTS = [
    "handshake",
    "diagnose",
    "message_hostnames",
    "event_dispatcher",
    "message_dispatcher",
    "lsp_request",
    "func_request",
    "open_file",
    "close_file",
    "close_all_files",
    "rename_file",
    "fetch_completion_item_info",
    "handle_file_elisp_message",
    "handle_lsp_message",
    "handle_remote_file_message",
    "open_remote_file",
    "update_remote_file",
    "save_remote_file",
    "close_remote_file",
    "sync_tramp_remote",
    "init_remote_file_server",
    "init_search_backends",
    "ctags_complete",
    "ctags_find_def",
    "copilot_complete",
    "copilot_login",
    "copilot_logout",
    "copilot_status",
    "copilot_completion_accept",
    "cleanup",
    "profile_dump",
    "start_test",
    "enjoy_hacking",
]


class Composition(unittest.TestCase):
    def test_the_three_mixins_are_bases_of_the_bridge(self):
        self.assertEqual(
            [base.__name__ for base in lsp_bridge.LspBridge.__mro__[1:4]],
            ["RemoteBridgeMixin", "ServerBridgeMixin", "SearchBridgeMixin"])

    def test_the_mixins_do_not_define_the_same_method_twice(self):
        seen = {}
        for mixin in (RemoteBridgeMixin, ServerBridgeMixin, SearchBridgeMixin):
            for name, _ in vars(mixin).items():
                if callable(getattr(mixin, name, None)) and not name.startswith("__"):
                    self.assertNotIn(
                        name, seen,
                        f"{name} is defined by both {seen.get(name)} and {mixin.__name__}")
                    seen[name] = mixin.__name__
        self.assertTrue(seen, "the mixins should define methods")

    def test_the_bridge_still_has_its_own_methods(self):
        own = {name for name, _ in vars(lsp_bridge.LspBridge).items()
               if callable(getattr(lsp_bridge.LspBridge, name, None)) and not name.startswith("__")}
        self.assertIn("open_file", own)
        self.assertIn("build_file_action_function", own)
        # ... and the ones that live in a mixin are inherited.
        self.assertNotIn("sync_tramp_remote", own)
        self.assertIn("sync_tramp_remote", dir(lsp_bridge.LspBridge))


class Surface(unittest.TestCase):
    def test_every_epc_entry_point_exists(self):
        missing = [name for name in EPC_ENTRY_POINTS
                   if not callable(getattr(lsp_bridge.LspBridge, name, None))]
        self.assertEqual(missing, [])

    def test_the_module_level_helpers_are_still_importable_from_the_bridge(self):
        # The Emacs side and the tests reach them through lsp_bridge.
        self.assertTrue(callable(lsp_bridge.read_lang_server_info))
        self.assertTrue(callable(lsp_bridge.load_single_server_info))
        self.assertTrue(callable(lsp_bridge.get_lang_server_path))

    def test_the_shared_decorator_and_channel_constants_moved_to_utils(self):
        from core import utils
        self.assertTrue(callable(utils.threaded))
        self.assertEqual(utils.REMOTE_FILE_SYNC_CHANNEL, 9999)
        self.assertEqual(utils.REMOTE_FILE_COMMAND_CHANNEL, 9998)
        self.assertEqual(utils.REMOTE_FILE_ELISP_CHANNEL, 9997)

    def test_the_bridge_file_only_keeps_its_own_methods(self):
        tree = ast.parse(BRIDGE.read_text(encoding="utf-8"))
        cls = next(node for node in tree.body
                   if isinstance(node, ast.ClassDef) and node.name == "LspBridge")
        own = {node.name for node in cls.body if isinstance(node, ast.FunctionDef)}
        self.assertLess(len(own), 30, "the class should be smaller than it was")
        self.assertTrue(set(EPC_ENTRY_POINTS) - own, "some entries should be inherited now")


if __name__ == "__main__":
    unittest.main()
