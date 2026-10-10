#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# /// script
# dependencies = [
#   "epc",
#   "orjson",
#   "packaging",
#   "sexpdata",
#   "six",
#   "setuptools",
#   "paramiko",
#   "rapidfuzz",
#   "watchdog",
# ]
# ///

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
import platform
import queue
import shutil
import threading
import traceback
import json
import time
import socket

from functools import wraps
from pathlib import Path

from epc.server import ThreadingEPCServer

from core.fileaction import (create_file_action_with_single_server,
                             create_file_action_with_multi_servers,
                             FILE_ACTION_DICT, LSP_SERVER_DICT)
from core.lspserver import LspServer
from core.search_file_words import SearchFileWords
from core.search_list import SearchList
from core.search_paths import SearchPaths
from core.copilot import Copilot
from core.utils import *
from core.handler import *
from core.remote_file import (
    RemoteFileClient,
    DockerFileClient,
    FileSyncServer,
    FileElispServer,
    FileCommandServer,
    SendMessageException,
    ContainerConnectionException,
    save_ip,
    get_container_local_ip
)
from core.ctags import Ctags

from core.bridge_remote import RemoteBridgeMixin
from core.bridge_search import SearchBridgeMixin
from core.bridge_server import (ServerBridgeMixin, read_lang_server_info,
                                load_single_server_info, get_lang_server_path)




PROTOCOL_VERSION = 2
"""Version of the Emacs <-> Python protocol.

Both halves declare it and compare it during the handshake
(`build_handshake`); `scripts/check-protocol.py` refuses to let them drift
apart, and `lsp-bridge-diagnose` reports the pair.  Bump it whenever a change
makes an old Emacs side and a new Python side (or the other way round)
incompatible.
"""

CAPABILITIES = [
    "completion",
    "diagnostics",
    "code-action",
    "formatting",
    "call-hierarchy",
    "rename",
    "copilot",
    "ctags",
    "remote-file",
    "diagnose",
]
"""Feature groups the Python side provides over EPC.

A capability is only listed here when the Emacs side has a counterpart that
needs it, so the handshake can be used to tell a user why a command is
unavailable rather than failing somewhere in the middle of it.
"""


def build_handshake(emacs_protocol_version):
    """Return the reply of a handshake with EMACS_PROTOCOL_VERSION.

    The reply is a dict: ``protocol_version`` is what this side speaks,
    ``compatible`` says whether the two versions can talk at all,
    ``capabilities`` lists the feature groups of ``CAPABILITIES`` and the last
    two entries describe the process that answered.

    Args:
        emacs_protocol_version: Protocol version the Emacs side announced.
    """
    return {
        "protocol_version": PROTOCOL_VERSION,
        "compatible": int(emacs_protocol_version) == PROTOCOL_VERSION,
        "capabilities": list(CAPABILITIES),
        "python_version": platform.python_version(),
        "pid": os.getpid(),
    }


def build_diagnose(lsp_server_count, file_action_count):
    """Return the runtime facts `lsp-bridge-diagnose' reports.

    Args:
        lsp_server_count: Number of running language servers.
        file_action_count: Number of files the server currently tracks.
    """
    return {
        "protocol_version": PROTOCOL_VERSION,
        "capabilities": list(CAPABILITIES),
        "python_version": platform.python_version(),
        "pid": os.getpid(),
        "lsp_server_count": lsp_server_count,
        "file_action_count": file_action_count,
    }


class LspBridge(RemoteBridgeMixin, ServerBridgeMixin, SearchBridgeMixin):
    """Own the lsp-bridge Python process and its Emacs-facing EPC server.

    An instance boots the EPC server that exposes this object's methods to
    Emacs and then starts the threads behind it: the event loop, the LSP
    message loop, the per-file actions (see ``core.fileaction``), the search
    backends and, when ``args`` is non-empty, the clients that talk to remote
    servers.  ``__init__`` blocks forever on the event loop.

    Attributes:
        running_in_server: True when started with no arguments, i.e. when this
            process serves remote Emacs clients over the SSH/Docker channels.
        server: The ``ThreadingEPCServer`` whose registered instance is this
            object, i.e. the transport of every Emacs-to-Python call.
        event_queue: Queue consumed by `event_dispatcher`.
        message_queue: Queue of messages produced by ``core.lspserver``.
        client_dict: Live remote file clients keyed by ``host:port``.
        lsp_client_dict: Empty per-host LSP client map, not read elsewhere.
        host_names: SSH/Docker connection settings keyed by host name.
        host_ip_dict: Cache mapping a resolved IP back to its host name.
        thread_queue: Threads started by the ``threaded`` decorator.
    """

    def __init__(self, args):
        """Start every lsp-bridge thread, then block on the event loop.

        Args:
            args: Command-line arguments.  Empty on a remote server;
                otherwise ``args[0]`` is the port of the Emacs EPC server to
                connect back to.
        """
        # Check running environment.
        self.running_in_server = len(args) == 0
        if self.running_in_server:
            set_running_in_server()

        # Build EPC server.
        self.server = ThreadingEPCServer(('127.0.0.1', 0), log_traceback=True)
        self.server.allow_reuse_address = True
        self.server.register_instance(self)  # register instance functions let elisp side call

        # Start EPC server.
        self.server_thread = threading.Thread(target=self.server.serve_forever)
        self.server_thread.start()

        # Init variables.
        self.thread_queue = []
        self.client_dict = {}
        self.lsp_client_dict = {}
        self.host_names = {}
        self.host_ip_dict = {}

        # Init event loop.
        self.event_queue = queue.Queue()
        self.event_loop = threading.Thread(target=self.event_dispatcher)
        self.event_loop.start()

        self.sync_tramp_remote_complete_event = threading.Event()

        if self.running_in_server:
            self.init_search_backends_complete_event = threading.Event()
            # Start remote file server if lsp-bridge running in server.
            self.init_remote_file_server()
        else:
            # Init EPC client and search backends if lsp-bridge running in local.
            init_epc_client(int(args[0]))
            self.init_search_backends()
            eval_in_emacs('lsp-bridge--first-start', self.server.server_address[1])

        # event_loop never exit, simulation event loop.
        self.event_loop.join()

    def handshake(self, emacs_protocol_version):
        """Answer the protocol handshake, as a JSON string.

        Exported over EPC; called by `lsp-bridge--handshake' while the Emacs
        side starts the process.  See `build_handshake` for the reply.

        Args:
            emacs_protocol_version: Protocol version the Emacs side announced.
        """
        return json.dumps(build_handshake(emacs_protocol_version))

    def diagnose(self):
        """Report the runtime facts of this server, as a JSON string.

        Exported over EPC; called by the `lsp-bridge-diagnose' command.  See
        `build_diagnose` for the reply.
        """
        return json.dumps(build_diagnose(len(LSP_SERVER_DICT), len(FILE_ACTION_DICT)))






    def send_message_dispatcher(self, queue, port):
        """Send the messages queued on QUEUE to the remote hosts.

        Runs as a thread.  Each item is looked up with `get_socket_client` and
        handed to the client.  A failed send drops every cached client of that
        host, reconnects once and tells Emacs through
        ``lsp-bridge-remote-reconnect``.  Errors are logged and the loop keeps
        running.

        Args:
            queue: The sender queue to consume.
            port: Channel port shared by all clients served by this queue.
        """
        try:
            while True:
                data = queue.get(True)

                server_host = data["host"]
                client = self.get_socket_client(server_host, port)
                try:
                    client.send_message(data["message"])
                except SendMessageException as e:
                    # lsp-bridge process might has been restarted, making the orignal socket no longer valid.
                    logger.exception("Connection %s is broken, message %s, error %s", f"{server_host}:{port}", data["message"], e)
                    # remove all the clients for server_host from client_dict
                    # client will be created again when get_socket_client is called
                    for client_id in [key for key in self.client_dict.keys() if key.startswith(server_host + ":")]:
                        self.client_dict.pop(client_id, None)

                    message_emacs(f"try to recreate connection to {server_host}:{port}")
                    try:
                        client = self.get_socket_client(server_host, port)
                    except Exception as e:
                        # FATAL: unable to restore
                        logger.exception(e)
                    else:
                        # connection restored, try to send out the message
                        client.send_message(data["message"])
                        eval_in_emacs('lsp-bridge-remote-reconnect', server_host, True)
                except Exception as e:
                    logger.exception(e)
                finally:
                    queue.task_done()
        except Exception:
            logger.error(traceback.format_exc())







    # Functions for local to send request to remote server
    @threaded
    def lsp_request(self, remote_file_host, remote_file_path, method, args):
        """Forward an LSP request from remote Emacs to REMOTE_FILE_HOST.

        Exported over EPC; called by the Emacs function
        ``lsp-bridge-remote-send-lsp-request`` for buffers that live on a
        remote host (see ``lsp-bridge-call-file-api``).  A ``change_file``
        request is also sent on the file sync channel so that the remote
        content is up to date before METHOD runs there.

        Args:
            remote_file_host: Host or container that owns the file.
            remote_file_path: Path of the file on the remote host.
            method: File action name to run, e.g. ``find_define``.
            args: Positional arguments of that action, each passed through
                ``epc_arg_transformer``.
        """
        if method == "change_file":
            self.send_remote_message(
                remote_file_host, self.remote_file_sender_queue, {
                "command": "change_file",
                "server": remote_file_host,
                "path": remote_file_path,
                "args": list(map(epc_arg_transformer, args))
            })

        self.send_remote_message(
            remote_file_host, self.remote_file_command_sender_queue, {
            "command": "lsp_request",
            "server": remote_file_host,
            "path": remote_file_path,
            "method": method,
            "args": list(map(epc_arg_transformer, args))
        })

    @threaded
    def func_request(self, remote_file_host, remote_file_path, method, args):
        """Call a plain lsp-bridge method on the remote server.

        Exported over EPC; called by the Emacs function
        ``lsp-bridge-remote-send-func-request`` for RPCs that are not file
        actions.

        Args:
            remote_file_host: Host or container that owns the file.
            remote_file_path: Path of the file on the remote host.
            method: Name of the method to call on the remote ``LspBridge``.
            args: Positional arguments, each passed through
                ``epc_arg_transformer``.
        """
        self.send_remote_message(
            remote_file_host, self.remote_file_command_sender_queue, {
            "command": "func_request",
            "server": remote_file_host,
            "path": remote_file_path,
            "method": method,
            "args": list(map(epc_arg_transformer, args))
        })


    @threaded
    def handle_lsp_message(self, message):
        """Run the ``eval-in-emacs`` request a remote server sent back.

        Runs on a thread fed by ``remote_file_command_receiver_queue``.

        Args:
            message: Decoded reply of a remote command; an ``eval-in-emacs``
                reply carries the ``sexp`` evaluated by
                ``eval_sexps_in_emacs``.
        """
        if message["command"] == "eval-in-emacs":
            # Execute emacs command from remote server.
            eval_sexps_in_emacs(message["sexp"])

    @threaded
    def handle_file_elisp_message(self, message):
        """Answer an elisp RPC from a remote server and send the result back.

        Runs on a thread fed by ``remote_file_elisp_receiver_queue``.
        Understands ``get_emacs_func_result`` and ``get_emacs_vars``; anything
        else is logged and answered with None.  The answered message is put on
        ``remote_file_elisp_sender_queue`` for the originating host.

        Args:
            message: RPC request carrying ``host``, ``command``, ``method``
                and ``args``.
        """
        # Receive elisp RPC call from remote server.
        log_time(f"Receive server elisp RPC: {message}")

        host = message["host"]

        # Read elisp code from local Emacs, and sendback to remote server.
        if message["command"] == "get_emacs_func_result":
            result = get_emacs_func_result(message["method"], *message["args"])
        elif message["command"] == "get_emacs_vars":
            result = get_emacs_vars(message["args"])
        else:
            logger.error("Unsupported command %s", message["command"])
            result = None

        message["result"] = result
        self.send_remote_message(host, self.remote_file_elisp_sender_queue, message)

    # Functions for local handling
    def event_dispatcher(self):
        """Run the queued work on the bridge's single event-loop thread.

        Runs as a thread on ``event_queue``: a ``close_file`` item is handled
        by `_close_file`, and an ``action_func`` item names a bound method that
        is called with the queued arguments.  This is the thread every file
        action runs on, so a file action may block here.  Exceptions are
        logged.
        """
        try:
            while True:
                message = self.event_queue.get(True)

                if message["name"] == "close_file":
                    self._close_file(message["content"])
                elif message["name"] == "action_func":
                    (func_name, func_args) = message["content"]
                    getattr(self, func_name)(*func_args)

                self.event_queue.task_done()
        except Exception:
            logger.error(traceback.format_exc())

    def message_dispatcher(self):
        """Handle the messages the LSP servers put on ``message_queue``.

        Runs as a thread; ``server_process_exit`` items are handled by
        `handle_server_process_exit`, anything else is logged as unhandled.
        Started from `init_search_backends`.
        """
        try:
            while True:
                message = self.message_queue.get(True)
                if message["name"] == "server_process_exit":
                    self.handle_server_process_exit(message["content"])
                else:
                    logger.error("Unhandled lsp-bridge message: %s" % message)

                self.message_queue.task_done()
        except Exception:
            logger.error(traceback.format_exc())

    def rename_file(self, old_filepath, new_filepath):
        """Notify the file action of OLD_FILEPATH that it was renamed.

        Exported over EPC; called by the Emacs function
        ``lsp-bridge--rename-file-advisor`` through
        ``lsp-bridge-call-file-api``.  Does nothing when OLD_FILEPATH has no
        file action.

        Args:
            old_filepath: Path the buffer had before the rename.
            new_filepath: Path the buffer has now.
        """
        if is_in_path_dict(FILE_ACTION_DICT, old_filepath):
            get_from_path_dict(FILE_ACTION_DICT, old_filepath).rename_file(old_filepath, new_filepath)

    def fetch_completion_item_info(self, filepath, item_key, server_name):
        """Resolve completion item ITEM_KEY of SERVER_NAME for FILEPATH.

        Exported over EPC; called by the Emacs function
        ``lsp-bridge-fetch-completion-item-info`` to send a
        ``completionItem/resolve`` request that fills in the documentation and
        extra text edits of that candidate.  Does nothing when FILEPATH has no
        file action.

        Args:
            filepath: File the completion was inserted in.
            item_key: Opaque key of the completion item.
            server_name: Name of the LSP server that produced the item.
        """
        if is_in_path_dict(FILE_ACTION_DICT, filepath):
            get_from_path_dict(FILE_ACTION_DICT, filepath).completion_item_resolve(item_key, server_name)

    def open_file(self, filepath):
        """Start the language server that handles FILEPATH.

        Internal helper called by the generated file action workers inside
        ``event_dispatcher``.  It asks Emacs for the multi and single server of
        FILEPATH and then builds either a multi-server file action, a
        single-server file action, or, for ``.org`` files, a single-server
        action for the language of the source block at point (diagnostics are
        disabled for ``.org``).  LSP is turned off for the file when nothing
        can be started.

        Args:
            filepath: Absolute path of the file being opened.

        Returns:
            True when a file action was created, False otherwise.
        """
        project_path = get_project_path(filepath)
        multi_lang_server = get_emacs_func_result("get-multi-lang-server", project_path, filepath)

        # notify change workspace folder to copilot server
        if self.copilot.is_initialized:
            self.copilot.change_workspace_folder(project_path)

        if os.path.splitext(filepath)[-1] == '.org':
            single_lang_server = get_emacs_func_result("get-single-lang-server", project_path, filepath)
            # Point may leave the source block before this queued request runs.
            if not isinstance(single_lang_server, str) or not single_lang_server:
                return False
            lang_server_info = load_single_server_info(single_lang_server)
            # For project-based LSP servers, search upward for projectFiles (e.g. Cargo.toml)
            # so the LSP server gets the real project root, not just the git root.
            if "projectFiles" in lang_server_info:
                project_root = self.find_project_root(filepath, lang_server_info["projectFiles"])
                if project_root:
                    project_path = project_root
            #TODO support diagnostic
            lsp_server = self.create_lsp_server(filepath, project_path, lang_server_info, enable_diagnostics=False)
            if not lsp_server:
                return False
            create_file_action_with_single_server(filepath, lang_server_info, lsp_server)
        elif multi_lang_server:
            # Try to load multi language server when get-multi-lang-server return match one.
            multi_lang_server_path = get_lang_server_path(multi_lang_server, True)

            with open(multi_lang_server_path, encoding="utf-8", errors="ignore") as f:
                multi_lang_server_info = json.load(f)
                servers = self.pick_multi_server_names(multi_lang_server_info)

                # Load multi language server only when all language server commands exist.
                if self.check_multi_server_command(servers, filepath):
                    multi_servers = {}

                    for server_name in servers:
                        server_path = get_lang_server_path(server_name)

                        with open(server_path, encoding="utf-8", errors="ignore") as server_path_file:
                            lang_server_info = read_lang_server_info(server_path_file)
                            lsp_server = self.create_lsp_server(
                                filepath,
                                project_path,
                                lang_server_info,
                                server_name in multi_lang_server_info.get("diagnostics", []))
                            if lsp_server:
                                multi_servers[lang_server_info["name"]] = lsp_server
                            else:
                                return False

                    self.enjoy_hacking(servers, project_path)

                    create_file_action_with_multi_servers(filepath, multi_lang_server_info, multi_servers)
                else:
                    # Try load single language server if multi language server load failed.
                    single_lang_server = get_emacs_func_result("get-single-lang-server", project_path, filepath)

                    if single_lang_server:
                        return self.load_single_lang_server(project_path, filepath)
                    else:
                        self.turn_off(
                            filepath,
                            "ERROR: can't find all command of multi-server for {}, haven't found match single-server".format(filepath))

                        return False
        else:
            # Try to load single language server.
            return self.load_single_lang_server(project_path, filepath)

        return True

    def close_file(self, filepath):
        """Queue the closing of FILEPATH behind the pending LSP requests.

        Exported over EPC; called by the Emacs functions
        ``lsp-bridge-close-buffer-file`` and
        ``lsp-bridge--rename-file-advisor``.  The work is put on
        ``event_queue`` and done later by `_close_file`.

        Args:
            filepath: Path of the buffer being closed.
        """
        # Add queue, make sure close file after other LSP request.
        self.event_queue.put({
            "name": "close_file",
            "content": filepath
        })

    def _close_file(self, filepath):
        """Exit the file action registered for FILEPATH.

        Internal handler for the ``close_file`` items of ``event_queue``; does
        nothing when FILEPATH has no file action.
        """
        if is_in_path_dict(FILE_ACTION_DICT, filepath):
            get_from_path_dict(FILE_ACTION_DICT, filepath).exit()

    def close_all_files(self):
        """Exit every file action and language server and forget both.

        Empties ``FILE_ACTION_DICT`` and ``LSP_SERVER_DICT``.  Nothing in this
        repository calls it.
        """
        FILE_ACTION_DICT.clear()

        for lsp_server in LSP_SERVER_DICT.values():
            lsp_server.exit()
        LSP_SERVER_DICT.clear()

    def enjoy_hacking(self, servers, project_path):
        """Print and echo that SERVERS are now serving PROJECT_PATH.

        Args:
            servers: Names of the language servers that were started.
            project_path: Project (or file) the servers were started for.
        """
        # Notify user server is ready.
        print("Start lsp server ({}) for {}".format(", ".join(servers), project_path))

        message_emacs("Active {} '{}', enjoy hacking!".format(
            "project" if os.path.isdir(project_path) else "file",
            os.path.basename(project_path.rstrip(os.path.sep))))








    def build_file_action_function(self, name):
        """Install both EPC wrappers of the file action NAME.

        Sets ``self.<name>`` to the EPC facing function that is called from
        Emacs and merely queues the work on ``event_queue`` so Emacs is never
        blocked, and ``self._<name>`` to the worker that runs on the event
        loop.  The worker opens the file when needed (see `open_file`) and then
        forwards to that filepath's ``FileAction`` object in
        ``FILE_ACTION_DICT`` with ``action.call(name, *args)``, which sends the
        request to the LSP server registered for the file.

        Args:
            name: File action name, e.g. ``find_define`` or ``change_file``.
        """
        def _do(filepath, *args):
            """Run file action NAME for FILEPATH on the event loop thread."""
            if is_remote_path(filepath):
                return
            open_file_success = True

            if not is_in_path_dict(FILE_ACTION_DICT, filepath):
                open_file_success = self.open_file(filepath)  # _do is called inside event_loop, so we can block here.
            elif os.path.splitext(filepath)[-1] == '.org' and get_emacs_vars(['lsp-bridge-enable-org-babel'])[0]:
                # check weather need create new lsp server
                self.maybe_create_org_babel_server(filepath)

            if open_file_success:
                action = get_from_path_dict(FILE_ACTION_DICT, filepath)
                action.call(name, *args)

        setattr(self, "_{}".format(name), _do)

        def _do_wrap(*args):
            """Queue file action NAME with ARGS for the event loop."""
            # To prevent long-time calculations from blocking Emacs, we need to put the function into event loop.
            self.event_queue.put({
                "name": "action_func",
                "content": ("_{}".format(name), list(map(epc_arg_transformer, args)))
            })

        setattr(self, name, _do_wrap)








    def cleanup(self):
        """Close every remote client and the EPC client before exit.

        Exported over EPC; called by the Emacs function
        ``lsp-bridge--kill-python-process`` when Emacs shuts the process down.
        """
        self.close_client()
        close_epc_client()

    def start_test(self):
        """Start the lsp-bridge test suite against this instance.

        Exported over EPC; called by the Emacs function
        ``lsp-bridge-start-test`` defined in ``test/lsp-bridge-test.el``,
        which hands control to ``test.test.start_test``.
        """
        # Called from lsp-bridge-test.el to start test.
        from test.test import start_test
        start_test(self)

    def profile_dump(self):
        """Write this process' cProfile statistics to ``~/lsp-bridge.prof``.

        Exported over EPC; called by the Emacs command
        ``lsp-bridge-profile-dump``.  When profiling was never enabled the
        global ``profiler`` does not exist and Emacs is instead told how to
        turn profiling on.
        """
        try:
            global profiler
            profiler.dump_stats(os.path.expanduser("~/lsp-bridge.prof"))
            message_emacs("Output profile data to ~/lsp-bridge.prof, please use snakeviz open it.")
        except Exception:
            message_emacs("Set option 'lsp-bridge-enable-profile' to 't' and call lsp-bridge-restart-process, then call lsp-bridge-profile-dump again.")



if __name__ == "__main__":
    if len(sys.argv) >= 3:
        import cProfile
        profiler = cProfile.Profile()
        profiler.run("LspBridge(sys.argv[1:])")
    else:
        LspBridge(sys.argv[1:])
