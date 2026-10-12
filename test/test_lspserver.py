"""Tests for the parts of `core/lspserver.py` that are not a process.

An LSP server is a child process, and most of the module is about running it.  What
is around that is not: the messages of the JSON-RPC protocol are built as dicts
before they are queued, the URI of a document is worked out from its path, and the
retry of an inlay hint request is remembered by the id it was sent with.  Those are
asked here, with the queue and the process stood in for.  What is not asked is
starting a server, the reads and writes of its pipes, and the dispatch of its
answers, which need the server itself.
"""

import os
import queue
import tempfile
import unittest
from unittest import mock

from core.lspserver import (
    INLAY_HINT_REQUEST_ID_DICT,
    LspServer,
    LspServerSender,
    MultiFileHandler,
    record_inlay_hint_request,
    resend_inlay_hint_request_after_content_modified_error,
)
from core.utils import path_to_uri


def make_server(**attributes):
    """Return an LspServer without running the constructor of the real one."""
    server = object.__new__(LspServer)
    for name, value in attributes.items():
        setattr(server, name, value)
    return server


def make_sender():
    """Return a sender whose process and queues are stood in for."""
    return LspServerSender(mock.Mock(), "test-server", "test-project")


class InlayHintRequestTest(unittest.TestCase):
    """The document of an inlay hint request is remembered for its retry."""

    def setUp(self):
        INLAY_HINT_REQUEST_ID_DICT.clear()

    def test_a_request_is_remembered(self):
        record_inlay_hint_request({"method": "textDocument/inlayHint", "id": 7,
                                   "params": {"textDocument": {"uri": "file:///tmp/a.py"}}})
        self.assertEqual(INLAY_HINT_REQUEST_ID_DICT[7], "file:///tmp/a.py")

    def test_another_request_is_not(self):
        record_inlay_hint_request({"method": "textDocument/hover", "id": 7,
                                   "params": {"textDocument": {"uri": "file:///tmp/a.py"}}})
        self.assertEqual(INLAY_HINT_REQUEST_ID_DICT, {})

    def test_a_malformed_request_is_ignored(self):
        # The request has no params to take a document from, and the exception it
        # raises is swallowed rather than left to the reader's thread.
        record_inlay_hint_request({"method": "textDocument/inlayHint", "id": 7})
        self.assertEqual(INLAY_HINT_REQUEST_ID_DICT, {})

    def test_the_retry_asks_emacs_for_the_remembered_document(self):
        record_inlay_hint_request({"method": "textDocument/inlayHint", "id": 7,
                                   "params": {"textDocument": {"uri": "file:///tmp/a.py"}}})
        with mock.patch("core.lspserver.eval_in_emacs") as called:
            resend_inlay_hint_request_after_content_modified_error({"id": 7})
        called.assert_called_once_with("lsp-bridge-inlay-hint-retry", "file:///tmp/a.py")
        # The entry is dropped as the retry is asked for.
        self.assertEqual(INLAY_HINT_REQUEST_ID_DICT, {})


class MultiFileHandlerTest(unittest.TestCase):
    """The paths of watched files are kept by their absolute name."""

    def test_a_relative_path_is_keyed_by_its_absolute_form(self):
        handler = object.__new__(MultiFileHandler)
        watched = {}
        handler._add_to_dict(watched, "some/relative/path.py")
        self.assertEqual(watched[os.path.abspath("some/relative/path.py")], "some/relative/path.py")


class SenderMessageTest(unittest.TestCase):
    """The messages of the protocol are built before they are queued."""

    def setUp(self):
        self.sender = make_sender()
        while not self.sender.queue.empty():
            self.sender.queue.get()

    def test_a_request(self):
        self.sender.send_request("textDocument/hover", {"a": 1}, 3)
        message = self.sender.queue.get()
        self.assertEqual(message["jsonrpc"], "2.0")
        self.assertEqual(message["id"], 3)
        self.assertEqual(message["method"], "textDocument/hover")
        self.assertEqual(message["params"], {"a": 1})
        self.assertEqual(message["message_type"], "request")

    def test_a_notification(self):
        self.sender.send_notification("textDocument/didSave", {"a": 1})
        message = self.sender.queue.get()
        self.assertEqual(message["method"], "textDocument/didSave")
        self.assertEqual(message["message_type"], "notification")
        self.assertNotIn("id", message)

    def test_a_response(self):
        self.sender.send_response(3, {"ok": True})
        message = self.sender.queue.get()
        self.assertEqual(message["id"], 3)
        self.assertEqual(message["result"], {"ok": True})
        self.assertEqual(message["message_type"], "response")
        self.assertNotIn("method", message)

    def test_a_message_can_be_queued_for_the_initialization_queue(self):
        self.sender.send_request("initialize", {}, 1, init=True)
        self.assertTrue(self.sender.queue.empty())
        self.assertEqual(self.sender.init_queue.get()["method"], "initialize")


class DocumentUriTest(unittest.TestCase):
    """A document's URI, and the org file that pretends to be another one."""

    def test_a_plain_path(self):
        server = make_server(server_info={}, project_path="/tmp/project")
        self.assertEqual(server.get_document_uri("/tmp/project/main.py"),
                         path_to_uri("/tmp/project/main.py"))

    def test_an_org_file_without_a_virtual_file(self):
        server = make_server(server_info={}, project_path="/tmp/project")
        self.assertEqual(server.get_document_uri("/tmp/project/notes.org"),
                         path_to_uri("/tmp/project/notes.org"))

    def test_an_org_file_with_a_virtual_file(self):
        # The project path is a directory in this case: when it is not, the module
        # takes its parent, which is the shape a project root has for an org file.
        with tempfile.TemporaryDirectory() as project:
            server = make_server(server_info={"orgBabelVirtualFile": "src/main.rs"},
                                 project_path=project)
            self.assertEqual(server.get_document_uri(os.path.join(project, "notes.org")),
                             path_to_uri(os.path.join(project, "src/main.rs")))

    def test_an_external_link_is_used_as_it_is(self):
        server = make_server(server_info={}, project_path="/tmp/project")
        self.assertEqual(server.parse_document_uri("/tmp/a.py", "jdt://contents/A.class"),
                         "jdt://contents/A.class")

    def test_an_external_path_is_made_into_a_uri(self):
        server = make_server(server_info={}, project_path="/tmp/project")
        self.assertEqual(server.parse_document_uri("/tmp/a.py", "/tmp/b.py"),
                         path_to_uri("/tmp/b.py"))

    def test_without_an_external_link(self):
        server = make_server(server_info={}, project_path="/tmp/project")
        self.assertEqual(server.parse_document_uri("/tmp/a.py", None),
                         path_to_uri("/tmp/a.py"))


class ServerNameTest(unittest.TestCase):
    """The name of the server is the one it was made for."""

    def test_the_name(self):
        self.assertEqual(make_server(server_name="rust-analyzer").get_server_name(),
                         "rust-analyzer")


class LanguageIdTest(unittest.TestCase):
    """The language id of a file, which the user can take over."""

    def make(self, server_info, filepath="/tmp/project/a.py", project="/tmp/project"):
        server = make_server(server_info=server_info, project_path=project, server_name="test")
        fa = mock.Mock()
        fa.filepath = filepath
        return server, fa

    def test_the_user_function_wins_when_it_returns_a_string(self):
        server, fa = self.make({"languageId": "python"})
        with mock.patch("core.lspserver.get_emacs_func_result", return_value="tailwindcss"):
            self.assertEqual(server.get_language_id(fa), "tailwindcss")

    def test_the_configured_ids_are_used_by_extension(self):
        server, fa = self.make({"languageId": "python", "languageIds": {"py": "python3"}})
        with mock.patch("core.lspserver.get_emacs_func_result", return_value=None):
            self.assertEqual(server.get_language_id(fa), "python3")

    def test_the_extension_is_used_when_the_configured_id_is_empty(self):
        server, fa = self.make({"languageId": ""})
        with mock.patch("core.lspserver.get_emacs_func_result", return_value=None):
            self.assertEqual(server.get_language_id(fa), "py")

    def test_the_configured_id_is_used_otherwise(self):
        server, fa = self.make({"languageId": "python"})
        with mock.patch("core.lspserver.get_emacs_func_result", return_value=None):
            self.assertEqual(server.get_language_id(fa), "python")


class InitializationOptionsTest(unittest.TestCase):
    """What the server is told at initialization."""

    def test_the_configured_options_are_kept(self):
        server = make_server(server_info={"initializationOptions": {"a": 1}},
                             workspace_folder=None)
        self.assertEqual(server.get_initialization_options(), {"a": 1})

    def test_the_workspace_folder_is_added(self):
        server = make_server(server_info={"initializationOptions": {"a": 1}},
                             workspace_folder="/tmp/project")
        self.assertEqual(server.get_initialization_options(),
                         {"a": 1, "workspaceFolders": ["/tmp/project"]})

    def test_a_server_without_options_gets_the_folder_alone(self):
        server = make_server(server_info={}, workspace_folder="/tmp/project")
        self.assertEqual(server.get_initialization_options(),
                         {"workspaceFolders": ["/tmp/project"]})


class CapabilitiesTest(unittest.TestCase):
    """The client capabilities, merged with what a server asks for."""

    def test_the_configured_capabilities_are_merged_in(self):
        server = make_server(server_info={"capabilities": {"textDocument": {"hover": {"contentFormat": ["markdown"]}}}},
                             workspace_folder=None,
                             enable_diagnostics=True)
        capabilities = server.get_capabilities()
        hover = capabilities["textDocument"]["hover"]
        # `merge' is a deep one, and for a list that means the values are joined
        # rather than replaced: what the default ask for stays, and what the server
        # asks for is added to it.
        self.assertEqual(hover["contentFormat"], ["markdown", "plaintext"])
        self.assertTrue(hover["dynamicRegistration"])
        self.assertTrue(capabilities["textDocument"]["completion"]["completionItem"]["snippetSupport"])

    def test_diagnostics_follow_the_option(self):
        for enabled in (True, False):
            server = make_server(server_info={}, enable_diagnostics=enabled,
                                 workspace_folder=None)
            diagnostic = server.get_capabilities()["textDocument"]["diagnostic"]
            self.assertEqual(diagnostic["relatedInformation"], enabled)
            self.assertEqual(diagnostic["dataSupport"], enabled)


if __name__ == "__main__":
    unittest.main()
