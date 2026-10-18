#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (C) 2022 Andy Stewart
#
# Author:     Andy Stewart <lazycat.manatee@gmail.com>
# Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
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
import os
import pprint
import threading
import time
from typing import TYPE_CHECKING, Dict, Tuple

from core.handler import *
from core.handler import Diagnostic
from core.lspserver import LspServer
from core.utils import *

if TYPE_CHECKING:
    from lsp_bridge import LspBridge  # noqa: F401

def create_file_action_with_single_server(filepath, single_server_info, single_server, external_file_link=None):
    """Create a FileAction driven by one LSP server and register it.

    Args:
        filepath: File the action belongs to.
        single_server_info: Language server configuration dictionary.
        single_server: The LspServer instance serving FILEPATH.
        external_file_link: TRAMP link when the file belongs to a remote
            project, else None.

    Returns:
        The new FileAction, or None when FILEPATH is already open; the
        warning is only logged when the already open file is bound to a
        different server.
    """
    if is_in_path_dict(FILE_ACTION_DICT, filepath):
        if get_from_path_dict(FILE_ACTION_DICT, filepath).single_server != single_server:
            logger.warn("File {} is opened by different lsp server.".format(filepath))
        return

    action = FileAction(filepath, single_server_info, single_server, None, None, external_file_link)
    add_to_path_dict(FILE_ACTION_DICT, filepath, action)
    return action

def create_file_action_with_multi_servers(filepath, multi_servers_info, multi_servers, external_file_link=None):
    """Create a FileAction served by several LSP servers and register it.

    Args:
        filepath: File the action belongs to.
        multi_servers_info: Multi-server configuration dictionary mapping a
            method name to the server names that should handle it.
        multi_servers: Dictionary of server name to LspServer instance.
        external_file_link: TRAMP link when the file belongs to a remote
            project, else None.

    Returns:
        The new FileAction, which is also stored in FILE_ACTION_DICT.
    """
    action = FileAction(filepath, None, None, multi_servers_info, multi_servers, external_file_link)
    add_to_path_dict(FILE_ACTION_DICT, filepath, action)
    return action

class FileAction:
    """Per-file state machine that routes Emacs requests to LSP servers.

    lsp-bridge creates one FileAction for every opened file and keeps it in
    the global FILE_ACTION_DICT.  Emacs calls it through
    `LspBridge.build_file_action_function` (method name plus arguments) and
    each file's LSP servers keep a back reference used to deliver responses.

    A request normally travels `call` -> `send_request` ->
    `send_server_request` -> LspServer; the answer comes back through the
    LspServer, which hands it to a Handler, whose `process_response` updates
    this object and calls `eval_in_emacs`/`message_emacs`.  Some methods here
    are therefore driven from Emacs and others from the response handlers.

    Attributes:
        filepath: Absolute path of the file this action owns.
        single_server: LspServer for single-server files, else None.
        multi_servers: Dictionary of server name to LspServer, else None.
        handlers: One handler instance per Handler subclass, keyed by the
            name Emacs uses and not bound to any single server.
        method_handlers: Per server name, the handler instances bound to it.
        version: Document version sent with change notifications.
        request_dict: Initialized empty and unused by this class; the per
            server request tables live on LspServer.
        completion_items: Per server name, completion items keyed by item key.
        diagnostics: Per server name, the sorted diagnostics to render.
        code_actions: Per server name, the code actions collected so far.
        org_file: True when FILEPATH is an org file; its src blocks are
            handled by the servers in `org_lang_servers`.
    """

    def __init__(self, filepath, single_server_info, single_server, multi_servers_info, multi_servers, external_file_link):
        """Create the state for FILEPATH and initialize its LSP servers.

        Args:
            filepath: File this action owns.
            single_server_info: Language server configuration dictionary for
                a single-server file, else None.
            single_server: LspServer for a single-server file, else None.
            multi_servers_info: Multi-server configuration dictionary, else
                None.
            multi_servers: Dictionary of server name to LspServer for a
                multi-server file, else None.
            external_file_link: TRAMP link of a remote project, else None.

        All per-file caches start empty, one handler instance is created per
        Handler subclass into `handlers`, the Emacs variables listed in the
        call below are read into `self`, and `set_lsp_server` finishes the
        initialization.
        """
        self.single_server_info = single_server_info
        self.single_server: LspServer = single_server
        self.multi_servers = multi_servers
        self.multi_servers_info = multi_servers_info

        self.code_actions = {}
        self.code_action_counter = 0

        self.completion_item_resolve_key = None
        self.completion_items = {}

        self.diagnostics = {}
        self.diagnostics_ticker = {}

        self.external_file_link = external_file_link
        self.filepath = filepath

        self.last_change_cursor_time = -1.0
        self.last_change_file_time = -1.0

        self.request_dict = {}

        self.try_completion_timer = None

        self.version = 1

        self.org_file = os.path.splitext(filepath)[-1] == '.org'
        self.org_line_bias = None

        self.pull_diagnostic_timer = None
        self.last_diagnostic_version = None

        # We need multiple servers to handle org files
        self.org_lang_servers = {}
        self.org_server_infos = {}
        if self.org_file:
             self.org_lang_servers[self.single_server.server_name] = self.single_server
             self.org_server_infos[self.single_server.server_name] = self.single_server_info

        self.handlers: Dict[str, Handler] = dict()
        logger.debug("Handlers: " + pprint.pformat(Handler.__subclasses__()))
        for handler_cls in Handler.__subclasses__():
            self.handlers[handler_cls.name] = handler_cls(self)

        (self.enable_auto_import,
         self.completion_items_limit,
         self.completion_match_mode,
         self.completion_case_mode,
         self.completion_workspace_symbol_items_limit,
         self.insert_spaces,
         self.enable_push_diagnostics,
         self.push_diagnostic_idle,
         self.display_label_max_length,
         self.diagnostics_max_number) = get_emacs_vars([
             "acm-backend-lsp-enable-auto-import",
             "acm-backend-lsp-candidates-max-number",
             "acm-backend-lsp-match-mode",
             "acm-backend-lsp-case-mode",
             "acm-backend-lsp-workspace-symbol-candidates-max-number",
             "indent-tabs-mode",
             "lsp-bridge-enable-diagnostics",
             "lsp-bridge-diagnostic-fetch-idle",
             "acm-backend-lsp-candidate-max-length",
             "lsp-bridge-diagnostic-max-number"
        ])
        self.completion_block_kind_list = None
        self.insert_spaces = not self.insert_spaces

        self.set_lsp_server()

    def set_lsp_server(self):
        """Attach this file to its servers and build the handler tables.

        Fills `method_handlers` with one handler set per server, calls
        `LspServer.attach` for each server so responses can come back, pulls
        diagnostics and tells Emacs the prefix style and the server names of
        this file.
        """
        self.method_handlers = {}
        for lsp_server in self.get_lsp_servers():
            method_handlers_dict = {}
            for handler_cls in Handler.__subclasses__():
                method_handlers_dict[handler_cls.name] = handler_cls(self)

            self.method_handlers[lsp_server.server_info["name"]] = method_handlers_dict

            lsp_server.attach(self)

        self.pull_diagnostics()

        # Set acm-input-bound-style when opened file.
        if self.single_server_info is not None:
            eval_in_emacs("lsp-bridge-set-prefix-style", self.single_server_info.get("prefixStyle", "ascii"))

        eval_in_emacs("lsp-bridge-set-server-names", self.filepath, get_lsp_file_host(), self.get_lsp_server_names())

    @property
    def last_change(self) -> Tuple[float, float]:
        """Return the last file change and cursor change timestamps.

        Handlers snapshot this tuple and drop their response when it no
        longer matches, which cancels requests made stale by newer edits.
        """
        return self.last_change_file_time, self.last_change_cursor_time

    def read_file(self):
        """Return the current content of this file.

        Org files are read from the Emacs buffer through
        `get_buffer_content`, every other file is read from disk with
        undecodable bytes ignored.
        """
        file_content = ''
        if self.org_file:
            file_content = get_buffer_content(self.filepath, os.path.basename(self.filepath))
        else:
            with open(self.filepath, encoding="utf-8", errors="ignore") as f:
                file_content = f.read()
        return file_content

    def get_method_server_names(self, method: str):
        """Return the names of the servers that should handle METHOD.

        Args:
            method: Emacs handler name, for example `completion` or
                `formatting`.

        Returns:
            List of server names.  For `completion`,
            `completion_item_resolve`, `diagnostics` and `code_action` the
            configured value may itself be a list; every other method is read
            as a single name wrapped in a list.  Both fall back to the
            `default` entry of the multi-server configuration.
        """
        default = self.multi_servers_info.get("default", None)
        if method in ["completion", "completion_item_resolve", "diagnostics", "code_action"]:
            method_server_names = self.multi_servers_info.get(method, default)
            if not isinstance(method_server_names, list):
                method_server_names = [method_server_names]
        else:
            method_server_names = [self.multi_servers_info.get(method, default)]
        return method_server_names

    def call(self, method, *args, **kwargs):
        """Run the request that Emacs sent for this file.

        Args:
            method: Handler name or name of a method on this object.
            *args: Arguments that follow the method name from Emacs.
            **kwargs: Keyword arguments forwarded to the callee.

        The method is looked up in `handlers` first: with a single server
        `send_request` is called once, otherwise once per server that
        `get_method_server_names` selects.  `execute_command` is the
        exception and sends to the one server named by its first argument.
        A method that is not a handler is called on this object instead, and
        an unknown name does nothing.
        """
        if method in self.handlers:
            handler = self.handlers[method]
            if self.single_server:
                self.send_request(self.single_server, method, handler, *args, **kwargs)
            else:
                if method in ["execute_command"]:
                    # "execute_command" is trigger by code action, one time only need send request to *ONE* LSP server.
                    method_server_names = [args[0]] # first arguments if server name.
                else:
                    method_server_names = self.get_method_server_names(method)
                for method_server_name in method_server_names:
                    method_server = self.multi_servers[method_server_name]
                    self.send_request(method_server, method, handler, *args, **kwargs)
        elif hasattr(self, method):
            getattr(self, method)(*args, **kwargs)

    def send_request(self, method_server, method, handler, *args, **kwargs):
        """Send one request to METHOD_SERVER after checking its capability.

        Args:
            method_server: LspServer to send to.
            method: Name of the handler being sent.
            handler: Handler class or instance whose `provider` attribute
                names the server capability required by the request.
            *args: Arguments forwarded to `send_server_request`.
            **kwargs: Keyword arguments forwarded to `send_server_request`.

        A handler without a `provider` sends directly.  A handler whose
        capability is missing sends `provider_message` to Emacs instead, and
        a capability that is False skips the request silently.
        """
        if hasattr(handler, "provider"):
            method_provider = getattr(method_server, getattr(handler, "provider"), None)
            if method_provider is not None:
                if method_provider is not False:
                    self.send_server_request(method_server, method, *args, **kwargs)
            elif hasattr(handler, "provider_message"):
                message_emacs(getattr(handler, "provider_message"))
        else:
            self.send_server_request(method_server, method, *args, **kwargs)

    def change_file(self, start, end, range_length, change_text, position, before_char, buffer_name, prefix):
        """Record a buffer edit and ask the servers for completion.

        Args:
            start: Start position of the change as a line/character dict.
            end: End position of the change as a line/character dict.
            range_length: Length of the replaced range.
            change_text: Text that replaced the range.
            position: Cursor position after the change.
            before_char: Character before the cursor.
            buffer_name: Emacs buffer the change came from.
            prefix: Completion prefix at the cursor.

        Servers with incremental sync get `send_did_change_notification`, the
        others get `send_whole_change_notification` built from the buffer, and
        `version` is then incremented.  For org files the line numbers are
        rebased by `org_line_bias` first; an out-of-range rebase logs a
        warning and falls back to a full `update_file`.  Any completion timer
        still pending is cancelled and a new one is scheduled, so completion
        runs after the edit idle delay, or immediately when the file is
        served by a remote lsp-bridge process.  Diagnostics are pulled as
        well.
        """
        if self.org_file:
            if self.org_line_bias is None:
                return
            start['line'] -= self.org_line_bias
            end['line'] -= self.org_line_bias
            if start['line'] < 0 or end['line'] < 0:
                logger.warning(
                    "Skip incremental org didChange with invalid rebased range: "
                    "start=%s end=%s org_line_bias=%s buffer=%s",
                    start, end, self.org_line_bias, buffer_name
                )
                # Org src-block position tracking can drift after commands that
                # move point outside the cached block. Fall back to a full sync
                # so we do not send invalid negative positions to the LSP server.
                self.update_file(buffer_name, self.org_line_bias)
                return

        buffer_content = ''
        # Send didChange request to LSP server.
        for lsp_server in self.get_lsp_servers():
            if lsp_server.text_document_sync == 0:
                continue
            elif lsp_server.text_document_sync == 1:
                if not buffer_content:
                    buffer_content = get_buffer_content(self.filepath, buffer_name)
                lsp_server.send_whole_change_notification(self.filepath, self.version, buffer_content)
            else:
                lsp_server.send_did_change_notification(self.filepath, self.version, start, end, range_length, change_text)

        self.version += 1

        # Try cancel expired completion timer.
        if self.try_completion_timer is not None and self.try_completion_timer.is_alive():
            self.try_completion_timer.cancel()

        self.last_change_file_time = time.time()

        # Send textDocument/completion 100ms later.
        if self.completion_block_kind_list is None:
            (self.completion_block_kind_list, ) = get_emacs_vars(["acm-backend-lsp-block-kind-list"])
            if isinstance(self.completion_block_kind_list, list):
                self.completion_block_kind_list = list(map(lambda x: x.lower(), self.completion_block_kind_list))

        if position['line'] < 0:
            # TODO fix this from elisp happened in org source code completion
            # We can temporary fix this by ignore this case,
            # which will not influence the completion result.
            logger.error("Invalid position change file", start, end, range_length, change_text, position, before_char, buffer_name, prefix, self.org_line_bias)
        else:
            delay = 0 if is_running_in_server() else 0.1
            self.try_completion_timer = threading.Timer(delay, lambda : self.try_completion(position, before_char, prefix, self.version))
            self.try_completion_timer.start()

        # Pull diagnostics after file changes
        self.pull_diagnostics()

    def update_file(self, buffer_name, org_line_bias=None):
        """Sync the whole buffer content to every LSP server.

        Args:
            buffer_name: Emacs buffer to read the content from.
            org_line_bias: Number of lines the org src block starts at, stored
                on `self` for the rebasing done by `change_file` and
                `try_completion`.

        `version` is incremented and diagnostics are pulled afterwards.
        """
        self.org_line_bias = org_line_bias
        buffer_content = get_buffer_content(self.filepath, buffer_name)
        for lsp_server in self.get_lsp_servers():
            lsp_server.send_whole_change_notification(self.filepath, self.version, buffer_content)
        self.version += 1

        # Pull diagnostics after file update if server supports it
        self.pull_diagnostics()

    def pull_diagnostics(self):
        """Schedule a pull diagnostics request for the current version.

        Nothing is scheduled when diagnostics were already pulled for
        `self.version`; otherwise a pending pull is cancelled and a new timer
        runs `_do_pull_diagnostics` after `push_diagnostic_idle` seconds.
        `last_diagnostic_version` records the version that was scheduled.
        """
        if self.last_diagnostic_version == self.version:
            return

        self.last_diagnostic_version = self.version

        # Cancel any pending diagnostic pulls
        if self.pull_diagnostic_timer and self.pull_diagnostic_timer.is_alive():
            self.pull_diagnostic_timer.cancel()

        delay = self.push_diagnostic_idle
        self.pull_diagnostic_timer = threading.Timer(delay, self._do_pull_diagnostics)
        self.pull_diagnostic_timer.start()

    def _do_pull_diagnostics(self):
        """Send the pull diagnostics request to the selected servers.

        Runs from the timer started by `pull_diagnostics`: multi-server files
        use the servers configured for `diagnostics`, a single-server file
        uses its only server.  Servers without `diagnostic_provider` or with
        diagnostics disabled are skipped.
        """
        if self.multi_servers:
            for server_name in self.get_method_server_names('diagnostics'):
                lsp_server = self.multi_servers[server_name]
                if lsp_server.diagnostic_provider and lsp_server.enable_diagnostics:
                    self.send_request(lsp_server, 'diagnostic', Diagnostic, lsp_server.server_info["name"])
        else:
            if self.single_server.diagnostic_provider and self.single_server.enable_diagnostics:
                self.send_request(self.single_server, 'diagnostic', Diagnostic, self.single_server.server_info["name"])

    def try_completion(self, position, before_char, prefix, version=None):
        """Ask the completion servers for candidates at POSITION.

        Args:
            position: Cursor position as a line/character dict.
            before_char: Character before the cursor.
            prefix: Completion prefix at the cursor.
            version: Document version to send; when None the current
                `self.version` is used, because Emacs does not know it when it
                calls this directly through `lsp-bridge-popup-complete-menu`.

        For org files the position line is rebased by `org_line_bias` here
        instead of in `change_file`, so a direct call stays correct.  Every
        server configured for `completion` gets a completion request, and a
        `completion_workspace_symbol` request is added when the server
        supports workspace symbols.
        """
        # If we call try_completion from Elisp side, Emacs don't know the version of FileAction.
        # So we need fill version if it is None.
        if self.org_file:
            # update line bias in `try_completion` instead of `change_file`
            # to avoid mismatch when `try_completion` is called directly through `lsp-bridge-popup-complete-menu`
            if self.org_line_bias is None:
                return
            position['line'] -= self.org_line_bias

        if version is None:
            version = self.version

        if self.multi_servers:
            for lsp_server in self.multi_servers.values():
                if lsp_server.server_info["name"] in self.get_method_server_names("completion"):
                    self.send_server_request(lsp_server, "completion", lsp_server, position, before_char, prefix, version)

                    if lsp_server.workspace_symbol_provider:
                        self.send_server_request(lsp_server, "completion_workspace_symbol", lsp_server, prefix)
        else:
            self.send_server_request(self.single_server, "completion", self.single_server, position, before_char, prefix, version)

            if self.single_server.workspace_symbol_provider:
                self.send_server_request(self.single_server, "completion_workspace_symbol", self.single_server, prefix)

    def try_formatting(self, start, end, *args, **kwargs):
        """Format the range between START and END.

        Args:
            start: Start position of the range.
            end: End position of the range; when it equals START the whole
                document is formatted.
            *args: Arguments forwarded to the formatting handler.
            **kwargs: Keyword arguments forwarded to the formatting handler.

        Every server configured for `formatting` is asked, or the only server
        of a single-server file.
        """
        if self.multi_servers:
            for lsp_server in self.multi_servers.values():
                if lsp_server.server_info["name"] in self.get_method_server_names("formatting"):
                    if start == end:
                        self.send_request(lsp_server, "formatting", Formatting, *args, **kwargs)
                    else:
                        self.send_request(lsp_server, "rangeFormatting", RangeFormatting, start, end, *args, **kwargs)
        else:
            if start == end:
                self.send_request(self.single_server, "formatting", Formatting, *args, **kwargs)
            else:
                self.send_request(self.single_server, "rangeFormatting", RangeFormatting, start, end, *args, **kwargs)

    def try_code_action(self, *args, **kwargs):
        """Request code actions from every configured server.

        Args:
            *args: Arguments forwarded to `send_code_action_request`.
            **kwargs: Keyword arguments forwarded to
                `send_code_action_request`.

        `code_action_counter` is reset first, because `push_code_actions`
        waits until every expected server has answered.
        """
        self.code_action_counter = 0

        if self.multi_servers:
            for lsp_server in self.multi_servers.values():
                if lsp_server.server_info["name"] in self.get_method_server_names("code_action"):
                    self.send_code_action_request(lsp_server, *args, **kwargs)
        else:
            self.send_code_action_request(self.single_server, *args, **kwargs)

    def change_cursor(self, position):
        """Record that the cursor moved, cancelling stale responses.

        Args:
            position: New cursor position; it is not stored, only
                `last_change_cursor_time` is updated so handlers with
                `cancel_on_change` can drop responses made stale by the move.
        """
        self.last_change_cursor_time = time.time()

    def get_diagnostics_count(self):
        """Return the total number of recorded diagnostics."""
        return sum(len(diags) for diags in self.diagnostics.values())

    def get_diagnostics(self, hide_severities=None):
        """Return the recorded diagnostics for Emacs to render.

        Args:
            hide_severities: Iterable of severities to skip, or None to keep
                every diagnostic.

        Returns:
            List of diagnostics, each tagged with a `server-name` key.  Each
            stored dictionary is mutated in place, and the list stops after
            `diagnostics_max_number` entries.
        """
        diagnostics = []
        diagnostic_count = 0
        for server_name in self.diagnostics:
            for diagnostic in self.diagnostics[server_name]:
                if hide_severities and diagnostic.get("severity", 1) in hide_severities:
                    continue
                diagnostic["server-name"] = server_name
                diagnostics.append(diagnostic)

                diagnostic_count += 1

                if diagnostic_count >= self.diagnostics_max_number:
                    return diagnostics

        return diagnostics

    def list_diagnostics(self, hide_severities):
        """Show the recorded diagnostics of this file to the user.

        Args:
            hide_severities: Severities filtered out of the list.

        Tells Emacs when there is nothing to show, otherwise calls the
        `lsp-bridge-diagnostic--list` function in Emacs.
        """
        diagnostic_count = 0
        for server_name in self.diagnostics:
            diagnostic_count += len(self.diagnostics[server_name])

        if diagnostic_count == 0:
            message_emacs("No diagnostics found.")
        else:
            eval_in_emacs("lsp-bridge-diagnostic--list", self.get_diagnostics(hide_severities))

    def sort_diagnostic(self, diagnostic_a, diagnostic_b):
        """Compare two diagnostics by start and end position.

        Args:
            diagnostic_a: First diagnostic.
            diagnostic_b: Second diagnostic.

        Returns:
            -1, 0 or 1 so the function can be used with
            `functools.cmp_to_key`.
        """
        score_a = [diagnostic_a["range"]["start"]["line"],
                   diagnostic_a["range"]["start"]["character"],
                   diagnostic_a["range"]["end"]["line"],
                   diagnostic_a["range"]["end"]["character"]]
        score_b = [diagnostic_b["range"]["start"]["line"],
                   diagnostic_b["range"]["start"]["character"],
                   diagnostic_b["range"]["end"]["line"],
                   diagnostic_b["range"]["end"]["character"]]

        if score_a < score_b:
            return -1
        elif score_a > score_b:
            return 1
        else:
            return 0

    def record_diagnostics(self, diagnostics, server_name):
        """Store diagnostics pushed by SERVER_NAME and schedule rendering.

        Args:
            diagnostics: Diagnostic list as received from the server.
            server_name: Name of the server that pushed them.

        The list replaces `self.diagnostics[server_name]` after being sorted
        by `sort_diagnostic`, the per-server `diagnostics_ticker` is bumped,
        and when push diagnostics are enabled a timer renders them after
        `push_diagnostic_idle` seconds.
        """
        log_time("Record diagnostics from '{}' for file {}".format(server_name, os.path.basename(self.filepath)))

        # Record diagnostics data that push from LSP server.
        import functools
        self.diagnostics[server_name] = sorted(diagnostics, key=functools.cmp_to_key(self.sort_diagnostic))

        if server_name in self.diagnostics_ticker:
            self.diagnostics_ticker[server_name] += 1
        else:
            self.diagnostics_ticker[server_name] = 0

        # Try to push diagnostics to Emacs.
        if self.enable_push_diagnostics:
            push_diagnostic_ticker = self.diagnostics_ticker[server_name]
            push_diagnostic_timer = threading.Timer(
                self.push_diagnostic_idle,
                lambda : self.try_push_diagnostics(push_diagnostic_ticker, server_name))
            push_diagnostic_timer.start()

    def try_push_diagnostics(self, ticker, server_name):
        """Render the diagnostics of SERVER_NAME when TICKER is current.

        Args:
            ticker: Value `diagnostics_ticker[server_name]` had when the
                render timer was created.
            server_name: Server whose diagnostics are rendered.

        Diagnostics recorded while the user keeps typing are dropped, and the
        ticker is checked per server so several servers cannot overwrite each
        other's diagnostics.
        """
        # Only push diagnostics to Emacs when ticker is newest.
        # Drop all temporarily diagnostics when typing.
        #
        # Note:
        # We need to check diagnostics ticker separately based on LSP server name,
        # to avoid multiple LSP server conflict each other.
        if ticker == self.diagnostics_ticker[server_name]:
            eval_in_emacs("lsp-bridge-diagnostic--render",
                          self.filepath,
                          get_lsp_file_host(),
                          self.get_diagnostics(),
                          self.get_diagnostics_count())

    def record_dart_closing_labels(self, labels):
        """Show the Dart closing labels pushed by the server.

        Args:
            labels: Closing labels received from the Dart analysis server.
        """
        eval_in_emacs("lsp-bridge-dart-closing-labels--render", self.filepath, get_lsp_file_host(), labels)

    def push_code_actions(self, actions, server_name, action_kind):
        """Collect the code actions of one server and render them when all answered.

        Args:
            actions: Code actions received from the server.
            server_name: Name of the server that answered.
            action_kind: Kind requested from the server; passed on to
                `lsp-bridge-code-action--fix` so Emacs applies the actions the
                right way.

        The actions are stored in `self.code_actions` and
        `code_action_counter` is incremented.  Only when every expected
        server has answered are the actions from `get_code_actions` sent to
        Emacs, or an explanatory message shown when there is nothing to do.
        """
        log_time("Record actions from '{}' for file {}".format(server_name, os.path.basename(self.filepath)))
        self.code_actions[server_name] = actions

        self.code_action_counter += 1
        check_counter = len(self.get_method_server_names("code_action")) if self.multi_servers else 1

        # Only send code action when all LSP server has received response.
        if self.code_action_counter >= check_counter:
            self.code_action_counter = 0

            code_actions = self.get_code_actions()
            if len(code_actions) > 0:
                eval_in_emacs("lsp-bridge-code-action--fix", code_actions, action_kind)
            elif self.get_diagnostics_count() > 0:
                message_emacs("Please move cursor to error or warning, then execute 'lsp-bridge-code-action' again.")
            else:
                message_emacs("Fantastic, your code looks great! No further actions needed!")

    def get_code_actions(self):
        """Return the collected code actions of every server.

        Each action is tagged with a `server-name` key; servers that recorded
        None contribute nothing.
        """
        code_actions = []
        for server_name in self.code_actions:
            if self.code_actions[server_name] is not None:
                for code_action in self.code_actions[server_name]:
                    code_action["server-name"] = server_name
                    code_actions.append(code_action)

        return code_actions

    def send_code_action_request(self, lsp_server, range_start, range_end, action_kind):
        """Ask LSP_SERVER for code actions in the given range.

        Args:
            lsp_server: Server to request actions from.
            range_start: Start position of the selection.
            range_end: End position of the selection.
            action_kind: Kind of actions wanted, forwarded to the handler.

        Diagnostics are sent only when this server pushed them for the
        selection: a diagnostic is kept when its start line is at or after
        `range_start` and its end line is at or before `range_end`, and
        entries without a `range` key are always kept.
        """
        lsp_server_name = lsp_server.server_info["name"]

        diagnostics = []
        if lsp_server_name in self.diagnostics:
            for diagnostic in self.diagnostics[lsp_server_name]:
                if "range" in diagnostic and not (
                    diagnostic["range"]["start"]["line"] >= range_start["line"]
                    and diagnostic["range"]["end"]["line"] <= range_end["line"]
                ):
                    continue
                diagnostics.append(diagnostic)

        self.send_request(
            lsp_server, "code_action", CodeAction,
            lsp_server_name, diagnostics, range_start, range_end, action_kind
        )

    def save_file(self, buffer_name):
        """Tell every LSP server that this file was saved.

        Args:
            buffer_name: Emacs buffer that was saved; it is forwarded to the
                didSave notification, which attaches the buffer content when
                the server asked for it.
        """
        for lsp_server in self.get_lsp_servers():
            lsp_server.send_did_save_notification(self.filepath, buffer_name)

    def completion_item_resolve(self, item_key, server_name):
        """Resolve a completion item so Emacs can show its full details.

        Args:
            item_key: Key of the item in `completion_items`.
            server_name: Server that offered the item.

        `completion_item_resolve_key` is set to ITEM_KEY so
        `completion_item_update` can ignore answers for items the user has
        already left.  A server with `completion_resolve_provider` gets a
        `completion_item_resolve` request; otherwise, and also when the item
        is not stored, `completion_item_update` is called right away with the
        data already known, or with empty strings.
        """
        if server_name in self.completion_items:
            self.completion_item_resolve_key = item_key

            if item_key in self.completion_items[server_name]:

                if self.multi_servers:
                    method_server = self.multi_servers[server_name]
                else:
                    method_server = self.single_server

                if method_server.completion_resolve_provider:
                    self.send_server_request(method_server,
                                             "completion_item_resolve",
                                             item_key,
                                             server_name,
                                             self.completion_items[server_name][item_key])
                else:
                    item = self.completion_items[server_name][item_key]

                    self.completion_item_update(
                        item_key,
                        server_name,
                        item["documentation"] if "documentation" in item else "",
                        item["additionalTextEdits"] if "additionalTextEdits" in item else "")
            else:
                # Still update completion documentation/additionalTextEdits with empty if item not in completion_items.
                self.completion_item_update(item_key, server_name, "", "")

    def completion_item_update(self, item_key, server_name, documentation, additional_text_edits):
        """Send resolved documentation for ITEM_KEY to Emacs.

        Args:
            item_key: Key of the completion item the update belongs to.
            server_name: Server that offered the item.
            documentation: Documentation string or MarkupContent dictionary;
                a dictionary with a `kind` key is reduced to its `value`.
            additional_text_edits: Edits to apply when the item is accepted.

        The update is dropped when `completion_item_resolve_key` is not
        ITEM_KEY, which happens when the user already moved to another item.
        """
        if self.completion_item_resolve_key == item_key:
           if isinstance(documentation, dict):
               if "kind" in documentation:
                   documentation = documentation["value"]

           eval_in_emacs("lsp-bridge-completion-item--update",
                         {
                             "filepath": self.filepath,
                             "key": item_key,
                             "server": server_name,
                             "additionalTextEdits": additional_text_edits,
                             "documentation": documentation
                         },
                         get_lsp_file_host())

    def rename_file(self, old_filepath, new_filepath):
        """Tell the first LSP server that a file was renamed.

        Args:
            old_filepath: Path before the rename.
            new_filepath: Path after the rename.
        """
        self.get_lsp_servers()[0].send_did_rename_files_notification(old_filepath, new_filepath)

    def send_server_request(self, lsp_server, handler_name, *args, **kwargs):
        """Build and send one LSP request to LSP_SERVER.

        Args:
            lsp_server: Server that receives the request.
            handler_name: Handler name registered in `method_handlers` for
                that server.
            *args: Arguments passed to the handler's `process_request`.
            **kwargs: Keyword arguments passed to the handler's
                `process_request`.

        A fresh request id is generated and stored on the handler together
        with the current `last_change` and the server info, so the response
        can be matched and stale answers can be discarded.  The document URI
        is added when the handler asks for it.
        """
        handler: Handler = self.method_handlers[lsp_server.server_info["name"]][handler_name]

        handler.latest_request_id = request_id = generate_request_id()
        handler.last_change = self.last_change
        handler.server_info = lsp_server.server_info

        lsp_server.record_request_id(request_id, handler)

        params = handler.process_request(*args, **kwargs)
        if handler.send_document_uri:
            params["textDocument"] = {"uri": lsp_server.parse_document_uri(self.filepath, self.external_file_link)}

        lsp_server.sender.send_request(
            method=handler.method,
            params=params,
            request_id=request_id)

    def exit(self):
        """Close this file on its servers and drop it from FILE_ACTION_DICT.

        Org files are closed on every server in `org_lang_servers`, other
        files on `get_lsp_servers`.  The server used is looked up again in
        the global LSP_SERVER_DICT by name.
        """
        for lsp_server in (self.org_lang_servers.values() if self.org_file else self.get_lsp_servers()):
            if lsp_server.server_name in LSP_SERVER_DICT:
                lsp_server = LSP_SERVER_DICT[lsp_server.server_name]
                lsp_server.close_file(self.filepath)

        # Clean FILE_ACTION_DICT after close file.
        remove_from_path_dict(FILE_ACTION_DICT, self.filepath)

    def get_lsp_servers(self):
        """Return the LSP servers of this file.

        Returns:
            The values of `multi_servers` for a multi-server file, otherwise a
            list holding the only server.
        """
        return self.multi_servers.values() if self.multi_servers else [self.single_server]

    def get_lsp_server_names(self):
        """Return the `name` of every LSP server of this file."""
        return list(map(lambda lsp_server: lsp_server.server_info["name"], self.get_lsp_servers()))

    def get_lsp_server_project_path(self):
        """Return the project path of the single server as bytes."""
        return self.single_server.project_path.encode('utf-8')

    def get_match_lsp_servers(self, method):
        """Return the servers configured to handle METHOD.

        Args:
            method: Emacs handler name looked up in the multi-server
                configuration.

        Returns:
            The LspServer objects for the configured names, or the only server
            of a single-server file.
        """
        if self.multi_servers:
            server_names = self.get_method_server_names(method)    # type: ignore
            return list(map(lambda server_name: self.multi_servers[server_name], server_names))    # type: ignore
        else:
            return [self.single_server]

    def create_external_file_action(self, external_file, external_file_link=None):
        """Create a FileAction for a file outside this project.

        Args:
            external_file: Path of the external file.
            external_file_link: TRAMP link of the external file, when it lives
                on a remote host.

        The new action reuses the servers of this file, so the external file
        is opened with the same language servers; it is registered in
        FILE_ACTION_DICT by the factory functions.
        """
        if self.multi_servers:
            create_file_action_with_multi_servers(external_file, self.multi_servers_info, self.multi_servers, external_file_link)
        else:
            create_file_action_with_single_server(external_file, self.single_server_info, self.single_server, external_file_link)

FILE_ACTION_DICT: Dict[str, FileAction] = {}  # use for contain file action
LSP_SERVER_DICT: Dict[str, LspServer] = {}  # use for contain lsp server
