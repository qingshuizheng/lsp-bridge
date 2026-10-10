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

def threaded(func):
    """Run FUNC on a freshly started thread instead of the calling one.

    The wrapper starts FUNC with the same arguments in a new
    ``threading.Thread`` and returns at once.  When the first positional
    argument exposes a ``thread_queue`` list, the new thread is appended to it
    so that its owner can join the work later.

    Args:
        func: The callable to run in a background thread.

    Returns:
        The wrapper that starts the thread.
    """
    @wraps(func)
    def wrapper(*args, **kwargs):
        """Start FUNC in a thread and record it on the first argument."""
        thread = threading.Thread(target=func, args=args, kwargs=kwargs)
        thread.start()
        if hasattr(args[0], 'thread_queue'):
            args[0].thread_queue.append(thread)
    return wrapper

REMOTE_FILE_SYNC_CHANNEL = 9999
REMOTE_FILE_COMMAND_CHANNEL = 9998
REMOTE_FILE_ELISP_CHANNEL = 9997


class LspBridge:
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

    def message_hostnames(self):
        """Send the known remote host names to Emacs as a message.

        Exported over EPC; called by the Emacs command
        ``lsp-bridge-tramp-show-hostnames``.  The names are shown through
        ``message_emacs`` rather than returned.
        """
        message_emacs(f"host_names:{self.host_names}")

    # Functions for initialization
    def init_search_backends(self):
        """Create the search, ctags and copilot backends and their EPC entries.

        Builds the copilot client plus the file-words, list and path search
        backends and the ctags wrapper, registers one EPC function per file
        action (see `build_file_action_function`) and per search backend method
        (see `build_prefix_function`), applies the log level held by the Emacs
        variable ``lsp-bridge-log-level`` and starts the LSP message loop plus,
        when running locally, the remote file sender and receiver loops.
        Called from `__init__` when running locally and from
        ``core.remote_file.FileElispServer`` on a remote server.
        """
        # Init copilot
        self.copilot = Copilot()

        # Init search backends.
        self.search_file_words = SearchFileWords()
        self.search_list = SearchList()
        self.search_paths = SearchPaths()
        self.ctags = Ctags()

        # Build EPC interfaces.
        handler_subclasses = list(map(lambda cls: cls.name, Handler.__subclasses__()))
        for name in ["change_file", "update_file",  "save_file",
                     "try_completion", "try_formatting",
                     "change_cursor",
                     "list_diagnostics",
                     "try_code_action",
                     "workspace_symbol"] + handler_subclasses:
            self.build_file_action_function(name)

        search_backend_export_functions = {
            "search_file_words": ["index_files", "change_buffer", "load_file", "close_file", "search"],
            "search_list": ["search", "update"],
            "search_paths": ["search"]
        }
        for search_backend, export_functions in search_backend_export_functions.items():
            for name in export_functions:
                self.build_prefix_function(search_backend, search_backend, name)

        [lsp_server_log_level] = get_emacs_vars(["lsp-bridge-log-level"])
        lsp_server_log_level = str(lsp_server_log_level)
        if lsp_server_log_level in ["debug", "warning", "error", "critical"]:
            log_level_dict = {
                "debug": logging.DEBUG,
                "warning": logging.WARNING,
                "error": logging.ERROR,
                "critical": logging.CRITICAL,
            }
            logger.setLevel(log_level_dict[lsp_server_log_level])

        # All LSP server response running in message_thread.
        self.message_queue = queue.Queue()
        self.message_thread = threading.Thread(target=self.message_dispatcher)
        self.message_thread.start()

        if not self.running_in_server:
            remote_threads = {
                "send_message_dispatcher": {
                    # Build loop to open remote file.
                    "remote_file_sender_queue": REMOTE_FILE_SYNC_CHANNEL,
                    # Build loop to call remote Python command.
                    "remote_file_command_sender_queue": REMOTE_FILE_COMMAND_CHANNEL,
                    # Build loop to reply remote rpc command.
                    "remote_file_elisp_sender_queue": REMOTE_FILE_ELISP_CHANNEL
                },
                "receive_message_dispatcher": {
                    # Build loop to receive remote file to local Emacs.
                    "remote_file_receiver_queue": "handle_remote_file_message",
                    # Build loop to receive remote Python command response from remote server to local Emacs.
                    "remote_file_command_receiver_queue": "handle_lsp_message",
                    # Build loop to receive remote rpc from remote server to local Emacs.
                    "remote_file_elisp_receiver_queue": "handle_file_elisp_message"
                }
            }

            for dispatcher, args in remote_threads.items():
                for q, v in args.items():
                    newq = queue.Queue()
                    setattr(self, q, newq)
                    if not isinstance(v, int):
                        v = getattr(self, v)
                    threading.Thread(
                        target=getattr(self, dispatcher),
                        args=(newq, v)
                    ).start()

    @threaded
    def init_remote_file_server(self):
        """Start the servers a remote Emacs client connects to.

        Runs on a background thread in remote-server mode: it binds the file
        sync server, the file command server and the elisp RPC server on their
        fixed channels and registers this instance with
        ``set_lsp_bridge_server`` so the helpers in ``core.utils`` can reach
        it.
        """
        print("* Running lsp-bridge on remote server. "
              "Access files with 'lsp-bridge-open-remote-file' or 'find-file /docker:...'")

        # Build loop for remote files management.
        self.file_server = FileSyncServer("0.0.0.0", REMOTE_FILE_SYNC_CHANNEL)

        # Build loop for call remote command from local Emacs.
        # Start waiting for init_search_backends_complete_event
        self.file_command_server = FileCommandServer("0.0.0.0", REMOTE_FILE_COMMAND_CHANNEL, self)

        # Build loop for call local Emacs function from server.
        # Signal that init_search_backends_complete_event is done
        self.file_elisp_server = FileElispServer("0.0.0.0", REMOTE_FILE_ELISP_CHANNEL, self)

        set_lsp_bridge_server(self)

    # Functions for communication between local and remote server
    def get_socket_client(self, server_host, server_port, is_retry=False):
        """Return the remote file client for SERVER_HOST and SERVER_PORT.

        SERVER_HOST may be an IP address, a host name registered by
        `sync_tramp_remote`, or a Docker container name.  It is first
        translated through ``host_ip_dict`` and, for the IP addresses the
        remote server reports back, matched against an already connected host
        name on the same port.  IP hosts and registered names go to
        `_get_remote_file_client`; anything else is treated as a container
        name and goes to `_get_docker_file_client`.

        Args:
            server_host: IP address, registered host name or container name.
            server_port: Channel port of the remote lsp-bridge process.
            is_retry: Forwarded to `_get_remote_file_client`; true means the
                remote process must not be started a second time.

        Returns:
            A connected client, or None when no connection could be made.
        """
        # currently only support SSH and docker
        #
        # try to distinguish SSH remote file or docker remote file using server_host
        # if visiting docker remote file, container name is passed as server_host
        if server_host in self.host_ip_dict:
            server_host = self.host_ip_dict[server_host]

        if is_valid_ip(server_host):
            # When the remote server sends messages back, it tags them with the
            # SSH client's IP (from client_address).  If we used a ProxyCommand
            # hostname (not an IP) when registering in host_names, the IP won't
            # be found directly.  Try to reuse an existing client for the same
            # port under a different (hostname) key, and cache the IP-to-hostname
            # mapping for future lookups.
            if server_host not in self.host_names:
                # Check if we already have a connected client on this port
                # under a different hostname key.
                for client_key in self.client_dict:
                    if client_key.endswith(f":{server_port}"):
                        existing_host = client_key.rsplit(":", 1)[0]
                        if existing_host in self.host_names:
                            self.host_ip_dict[server_host] = existing_host
                            server_host = existing_host
                            break
            return self._get_remote_file_client(server_host, server_port, is_retry)
        elif server_host in self.host_names:
            # Non-IP hostname that was registered via sync_tramp_remote (e.g. ProxyCommand hosts)
            return self._get_remote_file_client(server_host, server_port, is_retry)
        else:
            # server_host is the container_name
            return self._get_docker_file_client(server_host, server_port)

    def _get_remote_file_client(self, server_host, server_port, is_retry):
        """Return a cached or newly opened SSH client for SERVER_HOST.

        Resolves SERVER_HOST against ``host_names`` and ``host_ip_dict``, asks
        Emacs to reconnect through ``lsp-bridge-remote-reconnect`` when the
        host is unknown and waits for `sync_tramp_remote` to finish.  On a
        channel error it starts the remote lsp-bridge process when the Emacs
        variable ``lsp-bridge-remote-start-automatically`` allows it.

        Args:
            server_host: Host name or IP address to connect to.
            server_port: Channel port of the remote lsp-bridge process.
            is_retry: True on the recursive call made after starting the remote
                process, so that a second channel error gives up instead.

        Returns:
            The client cached in ``client_dict``, or None on failure.
        """
        if server_host in self.host_ip_dict:
            server_host = self.host_ip_dict[server_host]

        if server_host not in self.host_names and is_valid_ip(server_host):
            for known_host, conf in self.host_names.items():
                if conf.get('hostname') == server_host:
                    self.host_ip_dict[server_host] = known_host
                    server_host = known_host
                    break

        if server_host not in self.host_names:
            message_emacs(f"{server_host} is not connected, try reconnect...")
            self.sync_tramp_remote_complete_event.clear()
            eval_in_emacs('lsp-bridge-remote-reconnect', server_host, True)
            self.sync_tramp_remote_complete_event.wait()
            message_emacs(f"{server_host} connected.")

        client_id = f"{server_host}:{server_port}"
        if client_id in self.client_dict:
            return self.client_dict[client_id]

        import paramiko

        ssh_conf = self.host_names[server_host]
        try:
            client = RemoteFileClient(
                ssh_conf,
                server_port,
                lambda message: self.receive_remote_message(message, server_port),
            )
        except paramiko.AuthenticationException:
            # cloud not login server
            message_emacs(f"login {ssh_conf} failed, please check *lsp-bridge*")
            return None

        try:
            client.create_channel()
        except paramiko.ChannelException:
            # Channel Exception indicates that we clould not established channel
            # the remote process may not exist, try to start the process
            if is_retry:
                return None

            [remote_start_automatically] = get_emacs_vars(["lsp-bridge-remote-start-automatically"])
            if not remote_start_automatically:
                message_emacs(f"please make sure `lsp_bridge.py` has start at server {server_host} (lsp-bridge-remote-start-automatically is disabled)")
                return None
            # try to start remote process if it does not exist
            message_emacs(f"Start lsp-bridge process on {server_host} automatically...")
            client.start_lsp_bridge_process()
            # wait a while for the remote process to be ready
            time.sleep(2)
            # if client is not None, it has been started and put into client_dict
            return self.get_socket_client(server_host, server_port, is_retry=True)
        else:
            client.start()
            self.client_dict[client_id] = client
            return client

    def _get_docker_file_client(self, container_name, server_port):
        """Return a cached or newly opened Docker client for CONTAINER_NAME.

        Args:
            container_name: Name of the running container.
            server_port: Channel port of the lsp-bridge process inside it.

        Returns:
            The started client, cached in ``client_dict``.

        Raises:
            ContainerConnectionException: When the container cannot be
                reached; a diagnostic is printed before it is re-raised.
        """
        client_id = f"{container_name}:{server_port}"
        if client_id in self.client_dict:
            return self.client_dict[client_id]

        try:
            client = DockerFileClient(
                container_name=container_name,
                server_port=server_port,
                callback=lambda message: self.receive_remote_message(message, server_port),
            )
        except ContainerConnectionException as e:
            print(f"Failed to connect {container_name}, is it running?")
            raise e
        else:
            client.start()
            self.client_dict[client_id] = client
            return client

    def send_remote_message(self, host, queue, message, wait=False):
        """Put MESSAGE for HOST on the sender QUEUE.

        Args:
            host: Remote host or container name the message is bound for.
            queue: One of the ``remote_file_*_sender_queue`` attributes.
            message: Payload, queued as ``{"host": host, "message": message}``.
            wait: When true, block until the sender loop drained the queue.
        """
        queue.put({"host": host, "message": message})
        if wait:
            queue.join()

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
        except:
            logger.error(traceback.format_exc())

    def receive_remote_message(self, message, server_port):
        """Route a MESSAGE received on SERVER_PORT to its receiver queue.

        Used as the callback of the remote file clients.  Messages arriving on
        an unknown port are dropped.

        Args:
            message: The decoded message sent by the remote server.
            server_port: Channel the message came in on.
        """
        if server_port == REMOTE_FILE_SYNC_CHANNEL:
            self.remote_file_receiver_queue.put(message)
        elif server_port == REMOTE_FILE_COMMAND_CHANNEL:
            self.remote_file_command_receiver_queue.put(message)
        elif server_port == REMOTE_FILE_ELISP_CHANNEL:
            self.remote_file_elisp_receiver_queue.put(message)

    def receive_message_dispatcher(self, queue, handle_remote_message):
        """Call HANDLE_REMOTE_MESSAGE for every item put on QUEUE.

        Runs as a thread; exceptions are logged and end the loop.

        Args:
            queue: The receiver queue to consume.
            handle_remote_message: Bound handler for a single message.
        """
        try:
            while True:
                message = queue.get(True)
                handle_remote_message(message)
                queue.task_done()
        except:
            logger.error(traceback.format_exc())

    def remote_sync(self, host, remote_connection_info):
        """Open a remote file session on HOST for REMOTE_CONNECTION_INFO.

        Sends the "Connect" handshake on the elisp channel the first time HOST
        is seen, then asks the remote process to sync with
        REMOTE_CONNECTION_INFO, which is the TRAMP method prefix of the remote
        path (for example ``/ssh:`` or ``/docker:``).

        Args:
            host: Remote host or container name.
            remote_connection_info: TRAMP method prefix of the remote path.
        """
        client_id = f"{host}:{REMOTE_FILE_ELISP_CHANNEL}"
        if client_id not in self.client_dict:
            # send "say hello" upon establishing the first connection
            self.send_remote_message(
                host, self.remote_file_elisp_sender_queue, "Connect", True)

            self.send_remote_message(
                host, self.remote_file_sender_queue, {
                    "command": "remote_sync",
                    "server": host,
                    "remote_connection_info": remote_connection_info,
                })

    @threaded
    def sync_tramp_remote(self, tramp_file_name, tramp_method, server_username, server_host, ssh_port, path):
        """Resolve a TRAMP file name and connect to the server behind it.

        Exported over EPC; called by the Emacs command
        ``lsp-bridge-sync-tramp-remote`` and run on a background thread.  For
        ``/ssh:`` and ``/rpc:`` names it fills in an ssh config entry (reading
        ``~/.ssh/config`` for aliases and resolving the host to an IP unless a
        ProxyCommand is configured), stores it in ``host_names`` and syncs the
        remote process.  For ``/docker:`` names it checks the container IP and
        syncs the container.  It finishes by calling
        ``lsp-bridge-update-tramp-file-info`` (plus
        ``lsp-bridge--setup-tramp-docker-buffer`` for Docker) in Emacs and by
        setting ``sync_tramp_remote_complete_event``; unsupported methods only
        report an error to Emacs.

        Args:
            tramp_file_name: Full TRAMP file name being opened.
            tramp_method: TRAMP method, e.g. ``ssh``, ``rpc`` or ``docker``.
            server_username: User from the TRAMP file name, may be empty.
            server_host: Host name, IP address or container name.
            ssh_port: Port from the TRAMP file name, may be empty.
            path: Path of the file inside the remote host.
        """

        # tramp_file_name format example:
        #   /ssh:user@ip#port:/path/to/file
        #   /docker:user@container:/path/to/file
        # see https://www.gnu.org/software/tramp/#File-name-syntax
        tramp_method_prefix = tramp_file_name.rsplit(":", 1)[0]

        # Treat /rpc: tramp method the same as /ssh: (rpc is an SSH-based transport)
        if tramp_method_prefix.startswith("/ssh") or tramp_method_prefix.startswith("/rpc"):
            alias = None
            # arguments are passed from emacs using standard TRAMP functions tramp-file-name-<field>
            if server_host in self.host_names:
                server_host = self.host_names[server_host]['hostname']
                ssh_conf = self.host_names[server_host]
            elif is_valid_ip(server_host):
                ssh_conf = {'hostname' : server_host}
            else:
                import paramiko
                alias = server_host
                ssh_config = paramiko.SSHConfig()
                with open(os.path.expanduser('~/.ssh/config')) as f:
                    ssh_config.parse(f)
                ssh_conf = ssh_config.lookup(alias)

                server_host = ssh_conf.get('hostname', server_host)

            if not is_valid_ip(server_host):
                # When a ProxyCommand is configured, we don't need to resolve the
                # hostname to an IP -- the proxy handles routing.  Use the hostname
                # directly as the identifier.
                if ssh_conf.get('proxycommand', None):
                    message_emacs(f"Using ProxyCommand for {server_host}, skip IP resolution")
                elif server_host in self.host_ip_dict:
                    server_ip = self.host_ip_dict[server_host]
                    message_emacs(f"Resolve {server_host} to {server_ip} from host-ip cache")
                    server_host = server_ip
                    ssh_conf['hostname'] = server_ip
                else:
                    # https://stackoverflow.com/a/2816838
                    server_ips = [ str(i[4][0]) for i in socket.getaddrinfo(server_host, 0)]
                    if not server_ips:
                        message_emacs(f"Could not resolve host {server_host}")
                    else:
                        server_ip = server_ips[0]
                        message_emacs(f"Resolve {server_host} to {server_ip}")
                        server_host = server_ip
                        self.host_ip_dict[server_host] = server_ip
                        ssh_conf['hostname'] = server_ip

            if not is_valid_ip(server_host) and not ssh_conf.get('proxycommand', None):
                message_emacs("HostName Must be IP format.")

            if server_username:
                ssh_conf['user'] = server_username
            elif 'user' not in ssh_conf:
                # Default to current system user instead of "root" when
                # no user is specified in the tramp path or SSH config.
                import getpass
                ssh_conf['user'] = getpass.getuser()
            if ssh_port:
                ssh_conf['port'] = ssh_port
            self.host_names[server_host] = ssh_conf
            if alias:
                self.host_names[alias] = ssh_conf

            tramp_connection_info = tramp_method_prefix + ":"
            self.remote_sync(server_host, tramp_connection_info)

            eval_in_emacs("lsp-bridge-update-tramp-file-info", tramp_file_name, tramp_connection_info, tramp_method, server_username, server_host, path)
            self.sync_tramp_remote_complete_event.set()

        elif tramp_method_prefix.startswith("/docker"):
            # when using tramp to connect to container, the tramp_file_name might be
            #
            # /docker:USER@CONTAINER:/path/to/file
            # /docker:CONTAINER:/path/to/file
            #
            # see https://git.savannah.gnu.org/git/tramp.git listp/tramp-container.el
            tramp_method_split = tramp_method_prefix.split(":")[1]
            if "@" in tramp_method_split:
                container_user, container_name = tramp_method_split.split("@")
            else:
                container_user = "root"
                container_name = tramp_method_split

            if not get_container_local_ip(container_name):
                message_emacs(f"Cloud not get local ip of container {container_name}, is it running?")
                return

            # only support local container
            self.host_names[container_name] = {
                "server_host": "127.0.0.1",
                "username": container_user
            }

            try:
                tramp_connection_info = tramp_method_prefix + ":"
                self.remote_sync(container_name, tramp_connection_info)
                # container_name is used for emacs buffer local variable lsp-bridge-remote-file-host
                eval_in_emacs("lsp-bridge-update-tramp-file-info", tramp_file_name, tramp_connection_info, tramp_method, container_user, container_name, path)
                eval_in_emacs("lsp-bridge--setup-tramp-docker-buffer", tramp_file_name)
                self.sync_tramp_remote_complete_event.set()
            except Exception as e:
                logger.exception(e)
                message_emacs(f"Connect {container_user}@{container_name} failed")
                raise e
        else:
            message_emacs(
                f"Failed to access {tramp_file_name}, unsupported tramp methd: {tramp_method_prefix[1:]}, "
                "please make sure `lsp_bridge.py` has started on server."
            )

    # Functions for local to manage remote files
    @threaded
    def open_remote_file(self, path, jump_define_pos):
        """Open the remote PATH and jump to JUMP_DEFINE_POS.

        Exported over EPC; called from Emacs by ``lsp-bridge-open-remote-file``,
        ``lsp-bridge-define--jump`` and ``lsp-bridge-ref-open-file``, run on a
        background thread.  ``ip:path``
        and ``ip:port:path`` notations are split by ``split_ssh_path`` and
        ``/docker:`` notations by ``split_docker_path``; the host found that
        way is registered in ``host_names`` and an ``open_file`` command is
        sent to the remote process.  Unsupported path syntaxes only report an
        error to Emacs.

        Args:
            path: Remote path in one of the supported notations.
            jump_define_pos: Plist position to jump to, passed through
                ``epc_arg_transformer`` before it is forwarded.
        """
        if is_valid_ip_path(path):
            # open_remote_file path notation
            # ip:path
            # ip:port:path
            path_info = split_ssh_path(path)
            if path_info:
                (remote_info, server_host, ssh_conf, server_path) = path_info

                self.host_names[server_host] = ssh_conf

                self.remote_sync(server_host, f"/{remote_info}")

                message_emacs(f"Open {remote_info}{server_path}...")
                # Add TRAMP-related fields
                # The following fields: tramp_method, user, server, port, and path
                # are set as buffer-local variables in the buffer created by Emacs.
                # These variables facilitate the construction of a TRAMP file name,
                # and then allow the buffer to reconnect to a restarted remote lsp-bridge process
                # using the same logic with reconnecting a TRAMP remote file buffer.
                self.send_remote_message(
                    server_host, self.remote_file_sender_queue, {
                    "command": "open_file",
                    "tramp_method": "ssh",
                    "user": ssh_conf.get('user'),
                    "server": server_host,
                    "port": ssh_conf.get('port'),
                    "path": server_path,
                    "jump_define_pos": epc_arg_transformer(jump_define_pos)
                })
                save_ip(f"{remote_info}")
        elif path.startswith("/docker:"):
            # open_remote_file path notation
            # /docker:user@container:/path/to/file
            # /docker:container:/path/to/file
            path_info = split_docker_path(path)
            if path_info:
                (container_user, server_host, server_path) = path_info

                # only support local container
                self.host_names[server_host] = {
                    "server_host": "127.0.0.1",
                    "username": container_user
                }

                self.remote_sync(server_host, path.rsplit(":", 1)[0] + ":")

                message_emacs(f"Open {path}...")

                self.send_remote_message(
                    server_host, self.remote_file_sender_queue, {
                        "command": "open_file",
                        "tramp_method": "docker",
                        "user": container_user,
                        "server": server_host,
                        "port": None,
                        "path": server_path,
                        "jump_define_pos": epc_arg_transformer(jump_define_pos)
                })
        else:
            message_emacs(f"Unsupported remote path {path}")

    @threaded
    def update_remote_file(self, remote_file_host, remote_file_path, content):
        """Send the current CONTENT of a remote buffer to its host.

        Exported over EPC; called by the Emacs function
        ``lsp-bridge-format--update``.

        Args:
            remote_file_host: Host or container the buffer belongs to.
            remote_file_path: Path of the file on the remote host.
            content: Full text of the buffer.
        """
        self.send_remote_message(
            remote_file_host, self.remote_file_sender_queue, {
            "command": "update_file",
            "server": remote_file_host,
            "path": remote_file_path,
            "content": content
        })

    @threaded
    def save_remote_file(self, remote_file_host, remote_file_path):
        """Tell REMOTE_FILE_HOST that REMOTE_FILE_PATH was saved.

        Exported over EPC; called by the Emacs command
        ``lsp-bridge-remote-save-buffer``.

        Args:
            remote_file_host: Host or container the buffer belongs to.
            remote_file_path: Path of the file on the remote host.
        """
        self.send_remote_message(
            remote_file_host, self.remote_file_sender_queue, {
            "command": "save_file",
            "server": remote_file_host,
            "path": remote_file_path
        })

    @threaded
    def close_remote_file(self, remote_file_host, remote_file_path):
        """Tell REMOTE_FILE_HOST that REMOTE_FILE_PATH was closed.

        Exported over EPC; called by the Emacs function
        ``lsp-bridge-remote-kill-buffer``.

        Args:
            remote_file_host: Host or container the buffer belongs to.
            remote_file_path: Path of the file on the remote host.
        """
        self.send_remote_message(
            remote_file_host, self.remote_file_sender_queue, {
            "command": "close_file",
            "server": remote_file_host,
            "path": remote_file_path
        })

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

    # Functions for local to handle messages from remote server
    @threaded
    def handle_remote_file_message(self, message):
        """Handle an ``open_file`` reply from a remote server in Emacs.

        Runs on a thread fed by ``remote_file_receiver_queue``.  Reports
        ``message["error"]`` when the remote side failed, otherwise calls
        ``lsp-bridge-open-remote-file--response`` with the base64 encoded
        content and the target position.

        Args:
            message: Decoded reply of the remote ``open_file`` command.
        """
        command = message["command"]
        if command == "open_file":
            if "error" in message:
                message_emacs(message["error"])
            else:
                eval_in_emacs(
                    "lsp-bridge-open-remote-file--response",
                    message["tramp_method"],
                    message["user"],
                    message["server"],
                    message["port"],
                    message["path"],
                    string_to_base64(message["content"]),
                    message["jump_define_pos"],
                )
                message_emacs(f"Open file {message['path']} on {message['server']}")

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
        except:
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
        except:
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

    def load_single_lang_server(self, project_path, filepath):
        """Create a single-language-server file action for FILEPATH.

        Asks Emacs which server FILEPATH needs.  For servers that do not
        support single files the project root is searched through the server's
        ``projectFiles``; on any failure LSP is turned off for the file.

        Args:
            project_path: Project directory used to look the server up.
            filepath: Absolute path of the file being opened.

        Returns:
            True when a file action was created, False otherwise.
        """
        single_lang_server = get_emacs_func_result("get-single-lang-server", project_path, filepath)

        if not single_lang_server:
            self.turn_off(filepath, "ERROR: can't find the corresponding server for {}".format(filepath))

            return False

        lang_server_info = load_single_server_info(single_lang_server)

        # If project_path is file path, not dir.
        if ((not os.path.isdir(project_path)) and
            ("support-single-file" in lang_server_info and
             lang_server_info["support-single-file"] is False)):

            if "projectFiles" in lang_server_info:
                # If support-support-single-file is False,
                # we will search projectFiles up 20 level directories to find project root.
                project_root = self.find_project_root(filepath, lang_server_info["projectFiles"])
                if project_root is None:
                    self.turn_off_by_single_file(filepath, single_lang_server)

                    return False
                else:
                    # Replace project_path with project root path.
                    project_path = project_root
            else:
                self.turn_off_by_single_file(filepath, single_lang_server)

                return False

        lsp_server = self.create_lsp_server(filepath, project_path, lang_server_info)

        if lsp_server:
            self.enjoy_hacking([lang_server_info["name"]], project_path)

            create_file_action_with_single_server(filepath, lang_server_info, lsp_server)
        else:
            return False

        return True

    def find_project_root(self, filepath, project_files, max_depth=20):
        """Return the closest ancestor of FILEPATH holding a PROJECT_FILES file.

        Args:
            filepath: File whose ancestor directories are searched.
            project_files: Marker file names, e.g. ``Cargo.toml``.
            max_depth: How many parent directories to inspect at most.

        Returns:
            The directory as a string, or None when no marker was found.
        """
        current_dir = os.path.abspath(filepath)

        for _ in range(max_depth):
            for file in project_files:
                if os.path.isfile(os.path.join(current_dir, file)):
                    return current_dir

            parent_dir = os.path.dirname(current_dir)
            if parent_dir == current_dir: # reach topest dir
                break

            current_dir = parent_dir

        return None

    def turn_off(self, filepath, message):
        """Report MESSAGE to Emacs and disable LSP for FILEPATH.

        Does nothing for ``.txt`` files.  Emacs is told through
        ``lsp-bridge--turn-off-lsp-feature``.

        Args:
            filepath: File whose LSP support is disabled.
            message: Reason shown in the Emacs echo area.
        """
        if os.path.splitext(filepath)[1] != ".txt":
            message_emacs(message + ", disable LSP feature.")
            eval_in_emacs("lsp-bridge--turn-off-lsp-feature", filepath, get_lsp_file_host())

    def turn_off_by_single_file(self, filepath, single_lang_server):
        """Turn LSP off for FILEPATH because SINGLE_LANG_SERVER needs a project.

        Args:
            filepath: File whose LSP support is disabled.
            single_lang_server: Name or path of the unusable single server.
        """
        self.turn_off(
            filepath,
            "ERROR: {} not support single-file, you need put this file in a git repository or put .dir-locals.el in project root directory".format(single_lang_server))

    def check_lang_server_command(self, lang_server_info, filepath, turn_off_on_error=True):
        """Check that the command of LANG_SERVER_INFO can be started.

        The ``PATH`` from the Emacs variable ``exec-path`` is merged in first,
        and the command is replaced by the absolute path ``shutil.which``
        returned so the server can be started reliably.  On failure either LSP
        is turned off for FILEPATH or the error is only echoed, depending on
        TURN_OFF_ON_ERROR.

        Args:
            lang_server_info: Parsed language server JSON; its ``command`` list
                is modified in place.
            filepath: File the server would serve, used in error messages.
            turn_off_on_error: When true call `turn_off`, otherwise only call
                ``message_emacs``.

        Returns:
            True when the command was found, False otherwise.
        """
        # We merge PATH from `exec-path` variable, to make sure lsp-bridge find LSP server command if it can find by Emacs.
        merge_emacs_exec_path()

        if len(lang_server_info["command"]) > 0:
            server_command = lang_server_info["command"][0]
            server_command_path = shutil.which(server_command)

            if server_command_path:
                # We always replace LSP server command with absolute path of 'which' command.
                lang_server_info["command"][0] = server_command_path
                # message back the LSP server command path to emacs for verification
                # as some languages have runtime environment isolation
                # for example python virtualenv, NodeJS nvm, Ruby RVM
                message_emacs(f"found language server: {server_command_path}")
                return True
            else:
                error_message = "Error: can't find command '{}' to start LSP server {} ({})".format(
                    server_command, lang_server_info["name"], filepath)

                if turn_off_on_error:
                    self.turn_off(filepath, error_message)
                else:
                    message_emacs(error_message)

                return False
        else:
            error_message = "Error: {}'s command argument is empty".format(filepath)

            if turn_off_on_error:
                self.turn_off(filepath, error_message)
            else:
                message_emacs(error_message)

            return False

    def check_multi_server_command(self, server_names, filepath):
        """Check that every server in SERVER_NAMES has a runnable command.

        Errors are only echoed here, LSP is not turned off.

        Args:
            server_names: Names of the language servers to verify.
            filepath: File the servers would serve, used in error messages.

        Returns:
            True when all commands were found, False otherwise.
        """
        for server_name in server_names:
            server_path = get_lang_server_path(server_name)

            with open(server_path, encoding="utf-8", errors="ignore") as server_path_file:
                lang_server_info = read_lang_server_info(server_path_file)

                if not self.check_lang_server_command(lang_server_info, filepath, False):
                    return False

        return True

    def create_lsp_server(self, filepath, project_path, lang_server_info, enable_diagnostics=True):
        """Return the LspServer for PROJECT_PATH and LANG_SERVER_INFO.

        The server is created on first use and then reused from
        ``LSP_SERVER_DICT``, where it is keyed by project path and server name.

        Args:
            filepath: File the server serves, passed to the command check.
            project_path: Project directory the server is started for.
            lang_server_info: Parsed language server JSON.
            enable_diagnostics: Whether the server publishes diagnostics.

        Returns:
            The existing or new ``LspServer``, or False when its command is
            missing.
        """
        if not self.check_lang_server_command(lang_server_info, filepath):
            return False

        lsp_server_name = "{}#{}".format(path_as_key(project_path), lang_server_info["name"])

        if lsp_server_name not in LSP_SERVER_DICT:
            LSP_SERVER_DICT[lsp_server_name] = LspServer(
                message_queue=self.message_queue,
                project_path=project_path,
                server_info=lang_server_info,
                server_name=lsp_server_name,
                enable_diagnostics=enable_diagnostics)

        return LSP_SERVER_DICT[lsp_server_name]

    def pick_multi_server_names(self, multi_lang_server_info):
        """Return the language server names MULTI_LANG_SERVER_INFO refers to.

        Each top level value may be a single server name or a list of names;
        the result keeps the order of first appearance and drops duplicates.

        Args:
            multi_lang_server_info: Parsed multi-server JSON.

        Returns:
            The unique server names as a list.
        """
        servers = []
        for info in multi_lang_server_info:
            info_value = multi_lang_server_info[info]
            if isinstance(info_value, str):
                servers.append(info_value)
            else:
                servers += info_value

        return list(dict.fromkeys(servers))

    def maybe_create_org_babel_server(self, filepath):
        """Switch FILEPATH to the server of its current org source block.

        Called for ``.org`` files before a file action runs.  When the language
        Emacs reports for the block differs from the attached one, the matching
        server is created (or reused from ``action.org_lang_servers``),
        attached to the action and made the action's single server.  Does
        nothing when the language is unchanged or the server cannot be created.

        Args:
            filepath: The ``.org`` file being edited.
        """
        action = get_from_path_dict(FILE_ACTION_DICT, filepath)
        current_lang_server = get_emacs_func_result("get-single-lang-server",
                                                    action.single_server.project_path, filepath)
        lsp_server_name = "{}#{}".format(action.single_server.project_path, current_lang_server)
        if lsp_server_name != action.single_server.server_name and isinstance(current_lang_server, str):
            if lsp_server_name not in action.org_lang_servers:
                lang_server_info = load_single_server_info(current_lang_server)
                server = self.create_lsp_server(filepath, action.single_server.project_path,
                                                lang_server_info, enable_diagnostics=False)
                if not server:
                    return
                action.org_lang_servers[lsp_server_name] = server
                action.org_server_infos[lsp_server_name] = lang_server_info
                server.attach(action)
            else:
                lang_server_info = action.org_server_infos[lsp_server_name]
                server = action.org_lang_servers[lsp_server_name]
            action.single_server = server
            action.single_server_info = lang_server_info
            action.set_lsp_server()

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

    def build_prefix_function(self, obj_name, prefix, name):
        """Install an EPC wrapper that forwards to a backend method.

        Sets ``self.<prefix>_<name>`` to a function that calls
        ``getattr(self, obj_name).<name>`` with the same arguments, for example
        ``search_file_words_search`` forwards to
        ``self.search_file_words.search``.  This is how the search backends
        expose their methods to Emacs over EPC.

        Args:
            obj_name: Attribute of this object that holds the backend.
            prefix: EPC name prefix, normally the same as OBJ_NAME.
            name: Method name on the backend object.
        """
        def _do(*args, **kwargs):
            """Forward ARGS and KWARGS to the wrapped backend method."""
            getattr(getattr(self, obj_name), name)(*args, **kwargs)

        setattr(self, "{}_{}".format(prefix, name), _do)

    @threaded
    def ctags_complete(self, symbol, filename, max_lines, cursor_offset):
        """Ask ``self.ctags`` for completion candidates for SYMBOL.

        Exported over EPC; called by the Emacs function
        ``lsp-bridge-complete-other-backends`` and run on a background thread.
        The candidates are sent to Emacs by the ctags backend.

        Args:
            symbol: Symbol to complete.
            filename: File the completion happens in.
            max_lines: Maximum number of candidates to return.
            cursor_offset: Character offset of the cursor in the file.
        """
        self.ctags.make_complete(symbol, filename, max_lines, cursor_offset)

    @threaded
    def ctags_find_def(self, symbol, filename):
        """Ask ``self.ctags`` for the definition of SYMBOL in FILENAME.

        Exported over EPC; called by the Emacs command
        ``acm-backend-ctags-find-def-at-point`` and run on a background thread.

        Args:
            symbol: Symbol whose definition is looked up.
            filename: File the lookup happens in.
        """
        self.ctags.find_definition(symbol, filename)

    def copilot_complete(self, position, editor_mode, file_path, relative_path, tab_size, text, insert_spaces):
        """Ask the copilot backend for a completion at POSITION.

        Exported over EPC; called by the Emacs command
        ``lsp-bridge-copilot-complete``.  The candidates are sent to Emacs by
        ``self.copilot``.

        Args:
            position: Cursor position as sent by Emacs.
            editor_mode: Major mode of the buffer.
            file_path: Absolute path of the buffer.
            relative_path: Project relative path of the buffer.
            tab_size: Indentation width of the buffer.
            text: Text before the cursor.
            insert_spaces: Whether indentation uses spaces.
        """
        self.copilot.complete(position, editor_mode, file_path, relative_path, tab_size, text, insert_spaces)

    def copilot_login(self):
        """Start the copilot login flow through ``self.copilot``.

        Exported over EPC; called by the Emacs command
        ``lsp-bridge-copilot-login``.
        """
        self.copilot.login()

    def copilot_logout(self):
        """Log out of copilot through ``self.copilot``.

        Exported over EPC; called by the Emacs command
        ``lsp-bridge-copilot-logout``.
        """
        self.copilot.logout()

    def copilot_status(self):
        """Ask ``self.copilot`` to report its login status to Emacs.

        Exported over EPC; called by the Emacs command
        ``lsp-bridge-copilot-status``.
        """
        self.copilot.check_status()

    def copilot_completion_accept(self, id):
        """Tell the copilot backend that the completion ID was accepted.

        Exported over EPC; called by the Emacs function
        ``acm-backend-copilot-candidate-expand`` when
        ``acm-backend-copilot-accept`` is enabled.

        Args:
            id: Identifier of the accepted copilot completion.
        """
        self.copilot.accept(id)

    def handle_server_process_exit(self, server_name):
        """Forget the language server SERVER_NAME that exited.

        Internal handler for the ``server_process_exit`` items of
        ``message_queue``.

        Args:
            server_name: Key of the server in ``LSP_SERVER_DICT``.
        """
        if server_name in LSP_SERVER_DICT:
            log_time("Exit server {}".format(server_name))
            del LSP_SERVER_DICT[server_name]

    def close_client(self):
        """Kill the remote lsp-bridge process of every cached client."""
        for client in self.client_dict.values():
            if hasattr(client, "kill_lsp_bridge_process"):
                client.kill_lsp_bridge_process()

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
        except:
            message_emacs("Set option 'lsp-bridge-enable-profile' to 't' and call lsp-bridge-restart-process, then call lsp-bridge-profile-dump again.")

def read_lang_server_info(lang_server_path):
    """Return the LSP server configuration read from LANG_SERVER_PATH.

    The JSON is loaded and the ``command`` and ``initializationOptions`` fields
    have their ``%NAME%`` and ``$NAME`` placeholders expanded by
    ``replace_template``, so the result can be handed straight to
    ``LspServer``.

    Args:
        lang_server_path: Open file object holding the server JSON.

    Returns:
        The parsed and template-expanded configuration.
    """
    lang_server_info = json.load(lang_server_path)

    # Replace template in command options.
    lang_server_info["command"] = eval(replace_template(str(lang_server_info["command"])))

    # Replace template in initializationOptions.
    if "initializationOptions" in lang_server_info:
        lang_server_info["initializationOptions"] = eval(replace_template(str(lang_server_info["initializationOptions"])))

    return lang_server_info

def load_single_server_info(lang_server):
    """Return the configuration of the single language server LANG_SERVER.

    LANG_SERVER may be the path of a user JSON file, which is used directly, or
    the name of a server bundled under ``langserver/``, which is looked up with
    `get_lang_server_path`.

    Args:
        lang_server: Server name or path of a JSON configuration.

    Returns:
        The parsed configuration, as returned by `read_lang_server_info`.
    """
    lang_server_info_path = ""
    if isinstance(lang_server, str) and os.path.exists(lang_server) and os.path.dirname(lang_server) != "":
        # If lang_server is real file path, we load the LSP server configuration from the user specified file.
        lang_server_info_path = lang_server
    else:
        # Otherwise, we load LSP server configuration from file lsp-bridge/langserver/lang_server.json.
        lang_server_info_path = get_lang_server_path(lang_server)

    with open(lang_server_info_path, encoding="utf-8", errors="ignore") as f:
        return read_lang_server_info(f)

def get_lang_server_path(server_name, is_multi_server=False):
    """Return the JSON path that describes SERVER_NAME.

    A file from the directory configured in Emacs wins over the bundled one,
    and an OS specific ``<name>_<os>.json`` wins over the plain
    ``<name>.json``; the bundled path is returned when nothing else exists.

    Args:
        server_name: Name of the language or multi language server.
        is_multi_server: Look in ``multiserver/`` and the Emacs variable
            ``lsp-bridge-user-multiserver-dir`` instead of ``langserver/`` and
            ``lsp-bridge-user-langserver-dir``.

    Returns:
        A ``pathlib.Path``, which may not exist when the server is unknown.
    """
    lang_server_dir = "multiserver" if is_multi_server else "langserver"
    lang_server_dir_var = "lsp-bridge-user-multiserver-dir" if is_multi_server else "lsp-bridge-user-langserver-dir"

    server_dir = Path(__file__).resolve().parent / lang_server_dir
    server_path_current = server_dir / "{}_{}.json".format(server_name, get_os_name())
    server_path_default = server_dir / "{}.json".format(server_name)

    user_server_dir = Path(str(get_emacs_vars([lang_server_dir_var])[0])).expanduser()
    user_server_path_current = user_server_dir / "{}_{}.json".format(server_name, get_os_name())
    user_server_path_default = user_server_dir / "{}.json".format(server_name)

    if user_server_path_current.exists():
        server_path_current = user_server_path_current
    elif user_server_path_default.exists():
        server_path_current = user_server_path_default

    return server_path_current if server_path_current.exists() else server_path_default

if __name__ == "__main__":
    if len(sys.argv) >= 3:
        import cProfile
        profiler = cProfile.Profile()
        profiler.run("LspBridge(sys.argv[1:])")
    else:
        LspBridge(sys.argv[1:])
