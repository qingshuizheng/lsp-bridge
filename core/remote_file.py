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

import threading
import os
import glob
import json
import socket
import traceback
import time
from core.utils import *
from contextlib import contextmanager


class SendMessageException(Exception):
    """Raised when a remote message cannot be delivered to its peer."""
    pass


class ContainerConnectionException(Exception):
    """Raised when a Docker container socket cannot be reached."""
    pass


class RemoteFileClient(threading.Thread):
    """Talk to a remote lsp-bridge process over an SSH channel.

    One instance is created per SSH host and server port by `lsp_bridge.py`,
    which also passes the CALLBACK invoked for every message that arrives from
    the remote server.  Instances live in `LspBridge.client_dict` and are
    shared by all remote files of the same host.

    Attributes:
        chan: Paramiko channel to the remote lsp-bridge process, created by
            `create_channel`; `None` until then.
        remote_password_dict: Class level per-host password cache, filled in
            by `connect_ssh` so a password is only asked from Emacs once per
            session.
    """

    remote_password_dict = {}

    def __init__(self, ssh_conf, server_port, callback):
        """Open an SSH connection to the host described by SSH_CONF.

        Args:
            ssh_conf: Parsed SSH configuration dictionary for the host; its
                `hostname`, `user` and `port` keys select the login target.
            server_port: Port of the lsp-bridge process on the remote host.
            callback: Called with each parsed message read by `run`.

        The SSH connection is established here, but the channel to the remote
        lsp-bridge process is created later by `create_channel`.
        """
        threading.Thread.__init__(self)

        # Init.
        self.ssh_conf = ssh_conf
        self.ssh_host = ssh_conf['hostname']
        self.ssh_user = ssh_conf.get('user', "root")
        self.ssh_port = ssh_conf.get('port', 22)
        self.server_port = server_port
        self.callback = callback
        [self.remote_python_command, self.remote_python_file, self.remote_log] = get_emacs_vars(["lsp-bridge-remote-python-command", "lsp-bridge-remote-python-file", "lsp-bridge-remote-log"])

        [self.user_ssh_private_key,
         self.user_ssh_agent] = get_emacs_vars(["lsp-bridge-user-ssh-private-key",
                                                "lsp-bridge-user-ssh-agent"])

        self.ssh = self.connect_ssh(
            ssh_conf.get('gssapiauthentication', 'no') in ('yes'),
            ssh_conf.get('proxycommand', None)
        )
        # after successful login, don't create a channel yet
        # caller can use client ssh to execute command on remote server
        # ande then call create_channel() to create the channel
        self.chan = None

    def ssh_private_key(self):
        """Return the path of the SSH private key to login with, or None.

        Priority:
        1. Emacs variable `lsp-bridge-user-ssh-private-key`
        2. `identityfile` from the parsed SSH config
        3. First .pub file found in ~/.ssh/ (original fallback)
        """
        if self.user_ssh_private_key:
            return os.path.expanduser(self.user_ssh_private_key)

        # Use identityfile from SSH config if available
        identity_files = self.ssh_conf.get('identityfile', [])
        if identity_files:
            # paramiko returns identityfile as a list; use the first entry
            return os.path.expanduser(identity_files[0])

        ssh_dir = os.path.expanduser("~/.ssh")
        pub_keys = glob.glob(os.path.join(ssh_dir, "*.pub"))
        if pub_keys:
            return pub_keys[0][: -len(".pub")]

        return None

    def connect_ssh(self, use_gssapi, proxy_command):
        """Log in to `ssh_host` and return the connected paramiko client.

        Args:
            use_gssapi: Use GSSAPI authentication instead of a key or password.
            proxy_command: ProxyCommand to tunnel the connection through, or
                None to connect directly.

        Raises:
            paramiko.AuthenticationException: If the key login fails and the
                password login fails too.

        The key login failure is printed but not propagated, because the
        password login below is the intended fallback.
        """
        import paramiko

        ssh = paramiko.SSHClient()
        ssh.load_system_host_keys()
        ssh.set_missing_host_key_policy(paramiko.AutoAddPolicy())

        proxy = None
        if proxy_command:
            proxy = paramiko.ProxyCommand(proxy_command)

        try:
            if use_gssapi:
                ssh.connect(self.ssh_host, port=self.ssh_port, username=self.ssh_user, gss_auth=True, gss_kex=True, sock=proxy)
            else:
                # Login server with ssh private key.
                # Don't specify key_filename with ssh-agent (allow_agent is default to True)
                ssh_private_key = self.ssh_private_key() if not self.user_ssh_agent else None
                # look_for_keys defaults to True
                # when user specify the SSH private key path
                # disable searching for discoverable private key files in ~/.ssh/
                look_for_keys = not self.user_ssh_private_key
                ssh.connect(self.ssh_host, port=self.ssh_port, username=self.ssh_user, key_filename=ssh_private_key, look_for_keys=look_for_keys, sock=proxy)
        except:
            print(traceback.format_exc())

            # Try login server with password if private key is not available.
            try:
                # running `get-ssh-password` on macOS will raise error of 'The macOS Keychain auth-source backend doesn’t support creation yet'
                password = RemoteFileClient.remote_password_dict[self.ssh_host] if self.ssh_host in RemoteFileClient.remote_password_dict else get_ssh_password(self.ssh_user, self.ssh_host, self.ssh_port)

                ssh.connect(self.ssh_host, port=self.ssh_port, username=self.ssh_user, password=password)

                # Only remeber server's login password after login server successfully.
                # Password only record in memory for session login, not save in file.
                RemoteFileClient.remote_password_dict[self.ssh_host] = password
            except:
                print(traceback.format_exc())
                raise paramiko.AuthenticationException()

        return ssh

    def create_channel(self):
        """Open the channel to the remote lsp-bridge process.

        Raises:
            paramiko.ChannelException: If the remote lsp-bridge process does
                not exist.

        When the Emacs variable `lsp-bridge-remote-heartbeat-interval` is
        non-zero, a background thread running `heartbeat` is started so the
        channel is kept alive.
        """
        self.chan = self.ssh.get_transport().open_channel(
            "direct-tcpip", (self.ssh_host, self.server_port), ("0.0.0.0", 0)
        )
        if self.chan:
            [self.remote_heartbeat_interval] = get_emacs_vars(["lsp-bridge-remote-heartbeat-interval"])
            if self.remote_heartbeat_interval and self.remote_heartbeat_interval != 0:
                threading.Thread(target=self.heartbeat).start()

    def heartbeat(self):
        """Send `ping` on the channel every `remote_heartbeat_interval` seconds.

        Runs in its own thread started by `create_channel` and never returns;
        every failure is logged and ends the thread.
        """
        try:
            while True:
                self.chan.sendall("ping\n".encode("utf-8"))
                log_time_debug(f"Ping server: {self.ssh_host}, port: {self.server_port}")
                time.sleep(self.remote_heartbeat_interval)
        except Exception as e:
            logger.exception(e)

    def send_message(self, message):
        """Send MESSAGE to the remote server as one JSON line.

        Raises:
            SendMessageException: If the channel is invalid.

        Only socket errors are translated; the debug log is written after a
        successful send.
        """
        try:
            data = json.dumps(message)
            self.chan.sendall(f"{data}\n".encode("utf-8"))
        except socket.error as e:
            raise SendMessageException() from e
        else:
            log_time_debug(f"Sended to server {self.ssh_host} port {self.server_port}: {message}")

    def run(self):
        """Read messages from the channel and hand them to `callback`.

        This is the thread body: it reads newline-delimited JSON from the
        channel, passes each parsed message to the callback supplied at
        construction time, and closes the channel when the remote side closes
        it.
        """
        chan_file = self.chan.makefile("r")
        while True:
            data = chan_file.readline().strip()
            if not data:
                break

            message = parse_json_content(data)
            log_time_debug(f"Received from server {self.ssh_host} port {self.server_port}: {message}")
            self.callback(message)
        self.chan.close()

    def start_lsp_bridge_process(self):
        """Start the lsp-bridge process on the remote host when it is absent.

        Runs a login shell over SSH that reuses an already running process and
        otherwise launches `lsp-bridge-remote-python-command` with
        `lsp-bridge-remote-python-file`, redirecting output to
        `lsp-bridge-remote-log`; the local stdout and stderr of that command
        are printed.
        """
        remote_python_command = self.remote_python_command
        remote_python_file = self.remote_python_file
        remote_log = self.remote_log

        # use -l option to bash as a login shell, ensuring that login scripts (like ~/.bash_profile) are read and executed.
        # This is useful for lsp-bridge to use environment settings to correctly find out language server command
        _, stdout, stderr = self.ssh.exec_command(
            f"""
            nohup /bin/bash -l -c '
            pid=$(pgrep -f '\\''lsp_bridge.py$'\\'')
            if [ "$pid" == "" ]; then
                echo -e "Start lsp-bridge process as user $(whoami)" | tee >{remote_log}
                {remote_python_command} {remote_python_file} >>{remote_log} 2>&1 &
                if [ "$?" = "0" ]; then
                    echo -e "Start lsp-bridge successfully" | tee >>{remote_log}
                else
                    echo -e "Start lsp-bridge failed" | tee >>{remote_log}
                fi
            fi'
        """
        )
        print(f"Remote process started at {self.ssh_host}")
        print("stdout:" + stdout.read().decode())
        print("stderr:" + stderr.read().decode())

    def kill_lsp_bridge_process(self):
        """Kill the lsp-bridge process running on the remote host.

        The remote command logs to `lsp-bridge-remote-log` and this method
        swallows all errors on purpose: killing a process that is already gone
        must not break the caller.
        """
        remote_log = self.remote_log

        try:
            self.ssh.exec_command(
                f"""
                nohup /bin/bash -l -c '
                pid=$(pgrep -f '\\''lsp_bridge.py$'\\'')
                echo "try kill $pid" | tee >> {remote_log}
                if ! [ "$pid" == "" ]; then
                    echo -e "kill lsp-bridge process as user $(whoami)" | tee >>{remote_log}
                    kill $pid
                    if [ "$?" = "0" ]; then
                        echo -e "Kill lsp-bridge successfully" | tee >>{remote_log}
                    else
                        echo -e "Kill lsp-bridge failed" | tee >>{remote_log}
                    fi
                fi'
            """
            )
        except:
            pass


class DockerFileClient(threading.Thread):
    """Talk to a lsp-bridge process running inside a Docker container.

    The container is reached through a local TCP socket, so every message
    received by `run` is tagged with the container name before it reaches
    `callback`.
    """

    def __init__(self, container_name, server_port, callback):
        """Connect to the container and prepare the receiver thread.

        Args:
            container_name: Name of the Docker container, used as the host
                tag added to each received message.
            server_port: Port of the lsp-bridge process inside the container.
            callback: Called with each message parsed by `run`.
        """
        threading.Thread.__init__(self)
        self.container_name = container_name
        self.server_port = server_port
        self.callback = callback
        self.sock = None
        self.lock = threading.Lock()  # For thread-safe socket access

        self.connect()

    def connect(self):
        """Connect the socket to the container's lsp-bridge port.

        The socket is created under `lock` so concurrent senders cannot race
        during a reconnect.

        Raises:
            ContainerConnectionException: If the socket cannot be created or
                connected.
        """
        with self.lock:
            try:
                self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
                # connect to local host
                self.sock.connect(("127.0.0.1", self.server_port))
            except Exception as e:
                raise ContainerConnectionException(e)

    def send_message(self, message):
        """Send MESSAGE to the container as one JSON line.

        Raises:
            Exception: Whatever the socket raised after the failure is logged.

        A closed socket is reconnected once first; every failure is logged
        before it is re-raised.
        """
        try:
            if self.sock.fileno == -1:
                # container might be restarted, try to reconnect
                time.sleep(1)
                self.connect()

            data = json.dumps(message)
            self.sock.sendall(f"{data}\n".encode("utf-8"))

        except Exception as e:
            logger.exception(e)
            raise e
        else:
            log_time_debug(f"Sended to server {self.container_name} port {self.server_port}: {message}")

    def run(self):
        """Continuously listen for incoming data from the server.

        Each newline-delimited JSON message gets the container name added as
        its `host` key, then goes to `callback`.  A socket error is turned
        into SendMessageException and the socket is always closed.
        """
        try:
            sock_file = self.sock.makefile("r")
            while True:
                message = sock_file.readline().strip()
                if not message:
                    break

                message = parse_json_content(message)
                message["host"] = self.container_name
                log_time_debug(f"Received from server {self.container_name} port {self.server_port}: {message}")
                self.callback(message)

        except socket.error as e:
            raise SendMessageException() from e
        finally:
            self.sock.close()

class RemoteFileServer:
    """Base TCP server that serves the local Emacs side of remote editing.

    The local lsp-bridge process starts one subclass instance per channel
    (`FileSyncServer`, `FileCommandServer`, `FileElispServer`); each instance
    owns one listening socket and one `event_dispatcher` thread.

    Attributes:
        client_socket: Socket of the client handled right now, or None when
            no client is connected.
        client_address: `(host, port)` of that client, or None.
    """

    def __init__(self, host, port):
        """Bind HOST:PORT, then accept clients in the background.

        Args:
            host: Interface to bind, normally "0.0.0.0".
            port: TCP port of this channel.

        The listening socket is created with SO_REUSEADDR and the accept loop
        is started immediately in `event_dispatcher`.
        """
        import socket

        # Init.
        self.host = host
        self.port = port
        self.server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.server.bind((self.host, self.port))
        self.server.listen(5)

        # Build event loop.
        self.event_loop = threading.Thread(target=self.event_dispatcher)
        self.event_loop.start()

        self.client_socket = None
        self.client_address = None

    def event_dispatcher(self):
        """Accept clients forever and serve each one in its own thread.

        This is the thread body started by `__init__`.  Any exception is
        logged and stops the accept loop, because the thread has no way to
        report it to the caller.
        """
        try:
            while True:
                self.client_socket, self.client_address = self.server.accept()

                threading.Thread(target=self.handle_client).start()
        except Exception as e:
            logger.exception(e)

    def handle_client(self):
        """Serve the connected client until it disconnects.

        Reads one JSON message per line, ignores `ping` keep-alives, and
        writes the string returned by `handle_message` back to the client.
        The client socket is closed and `client_socket`/`client_address` are
        reset to None; any error is logged so one bad client cannot kill the
        server.
        """
        try:
            client_file = self.client_socket.makefile('r')
            while True:
                data = client_file.readline().strip()
                if not data:
                    break
                elif data == "ping":
                    log_time_debug(f"Server port {self.port} received ping from client {self.client_address}")
                    continue

                message = parse_json_content(data)
                log_time_debug(f"Server port {self.port} received message from client {self.client_address}: {message}")
                resp = self.handle_message(message)
                if resp:
                    self.client_socket.send(f"{resp}\n".encode("utf-8"))

            client_file.close()
            self.client_socket.shutdown(socket.SHUT_RDWR)
            self.client_socket.close()
            log_time(f"Server port {self.port} socket close for client {self.client_address}")
            self.client_socket = None
            self.client_address = None
        except Exception as e:
            logger.exception(e)

    def handle_message(self, message):
        """Handle one request from the client and return its response.

        Args:
            message: Parsed JSON request dictionary.

        Returns:
            Response string to send back, or None to send nothing.

        This base implementation is an extension point: it does nothing.
        """
        return

    def send_message(self, message):
        """Send MESSAGE to the current client socket.

        Raises:
            SendMessageException: If no client is connected or the send fails.

        The client IP is added to the message as its `host` key, so the local
        side can tell which remote host produced the message.
        """
        try:
            if self.client_socket:
                if self.client_address:
                    message["host"] = self.client_address[0]

                data = json.dumps(message)
                self.client_socket.send(f"{data}\n".encode("utf-8"))
        except Exception as e:
            logger.exception(e)
            raise SendMessageException() from e
        else:
            log_time_debug(f"Server port {self.port} sended to client {self.client_address}: {message}")


class FileSyncServer(RemoteFileServer):
    """Serve file access requests of the local Emacs for remote files.

    Runs on the remote host and keeps the last known content of every opened
    file in `file_dict`, so `handle_open_file` can answer without touching the
    disk and edits arriving as diffs can be applied in memory.

    Attributes:
        file_dict: Maps absolute file path to its current text content.
        file_locks: Maps absolute file path to the lock protecting it.
    """

    def __init__(self, host, port):
        """Start the file sync server with empty content and lock tables.

        Args:
            host: Interface to bind, normally "0.0.0.0".
            port: TCP port of the file sync channel.
        """
        super().__init__(host, port)
        self.file_dict = {}
        self.file_locks = {}

    def handle_message(self, message):
        """Dispatch a file request to the matching `handle_*` method.

        Args:
            message: Request dictionary whose `command` key selects the
                operation: `open_file`, `save_file`, `close_file`,
                `change_file`, `remote_sync` or `update_file`.

        Returns:
            Whatever the selected handler returns; None for an unknown
            command, for which no error is reported.
        """
        command = message["command"]

        if command == "open_file":
            return self.handle_open_file(message)
        elif command == "save_file":
            return self.handle_save_file(message)
        elif command == "close_file":
            return self.handle_close_file(message)
        elif command == "change_file":
            return self.handle_change_file(message)
        elif command == "remote_sync":
            return self.handle_remote_sync(message)
        elif command == "update_file":
            return self.handle_update_file(message)

    @contextmanager
    def file_access_lock(self, path):
        """Hold the per-path lock while the caller reads or writes PATH.

        Args:
            path: File path to lock, expanded to an absolute path first.

        Yields:
            None, with the lock for PATH held.

        The lock prevents `utils.get_buffer_content` and
        `utils.get_file_content_from_file_server` from reading the file
        content before `handle_open_file` has finished.
        """
        path = os.path.abspath(os.path.expanduser(path))

        #  use lock to prevent
        #    - utils.get_buffer_content
        #    - utils.get_file_content_from_file_server
        # from reading file content before handle_open_file finishes.
        lock = self.file_locks.setdefault(path, threading.Lock())
        try:
            lock.acquire()
            yield
        finally:
            lock.release()

    def get_file_content(self, path):
        """Return the cached content of PATH, waiting for a pending open.

        Args:
            path: File path whose content is requested.

        Returns:
            The file content, or an empty string when PATH is unknown.

        When PATH has not been read yet the call blocks on `file_access_lock`
        until `handle_open_file` has filled `file_dict`.
        """
        if path in self.file_dict:
            return self.file_dict[path]
        else:
            # wait for the lock if only path haven't been read
            with self.file_access_lock(path):
                return self.file_dict.get(path, "")

    def handle_update_file(self, message):
        """Store the content that the local Emacs pushed for a file.

        Args:
            message: Request with `path` and its new `content`.
        """
        path = message["path"]
        self.file_dict[path] = message["content"]

    def handle_remote_sync(self, message):
        """Store the remote connection info of the client in this process.

        Args:
            message: Request with `remote_connection_info`, the TRAMP prefix
                used to build remote paths.
        """
        remote_info = message["remote_connection_info"]
        set_remote_connection_info(remote_info)

    def handle_open_file(self, message):
        """Read a file on the remote host and return its content.

        Args:
            message: Request whose `path` is used to read the file and whose
                other keys, the TRAMP metadata, are copied into the response.

        Returns:
            JSON string with the file `content`, or with an `error` message
            when the file does not exist.  The content is also cached in
            `file_dict` under the read lock.
        """
        path = message["path"]
        response = {**message, "path": path}

        if os.path.exists(path):
            with self.file_access_lock(path):
                with open(path) as f:
                    content = f.read()
                    response.update({
                        "path": path,
                        "content": content
                    })
                    self.file_dict[path] = content
        else:
            response.update({
                "path": path,
                "content": "",
                "error": f"Cannot found file {path} on server.",
            })

        return json.dumps(response)

    def handle_change_file(self, message):
        """Apply an incremental change to the cached content of a file.

        Args:
            message: Request with `path` and `args` holding the change range,
                text and length used by `utils.rebuild_content_from_diff`.
        """
        path = message["path"]
        if path not in self.file_dict:
            with open(path) as f:
                self.file_dict[path] = f.read()

        self.file_dict[path] = rebuild_content_from_diff(self.file_dict[path], message["args"][0], message["args"][1], message["args"][3])

    def handle_save_file(self, message):
        """Write the cached content of a file back to disk.

        Args:
            message: Request with `path`; files that are not cached are
                ignored.
        """
        path = message["path"]

        if path in self.file_dict:
            with open(path, 'w') as file:
                file.write(self.file_dict[path])

    def handle_close_file(self, message):
        """Drop the cached content of a closed file.

        Args:
            message: Request with `path`; paths that are not cached are
                ignored.
        """
        path = message["path"]

        if path in self.file_dict:
            del self.file_dict[path]

    def close_all_files(self):
        """Forget every cached file content and every file lock."""
        self.file_dict.clear()
        self.file_locks.clear()


class FileElispServer(RemoteFileServer):
    """Relay RPC calls from the remote lsp-bridge process to local Emacs.

    This server runs in the local process.  The remote lsp-bridge connects to
    it and keeps the connection open; `call_remote_rpc` then sends a request
    over that connection and blocks until the answer comes back through
    `handle_message`.

    Attributes:
        rpcs: Maps request timestamp to the pending request dictionary, which
            holds the message, its result slot and the completion event.
    """

    def __init__(self, host, port, lsp_bridge):
        """Start the elisp RPC server with an empty pending-request table.

        Args:
            host: Interface to bind, normally "0.0.0.0".
            port: TCP port of the elisp channel.
            lsp_bridge: The LspBridge instance used to (re)initialize search
                backends when a client connects.
        """
        self.lsp_bridge = lsp_bridge
        self.rpcs = {}
        super().__init__(host, port)

    def handle_client(self):
        """Prepare a freshly connected remote client, then serve it.

        Discards the results of still pending RPCs, clears
        `init_search_backends_complete_event`, serves the client in a thread
        of its own and finally calls `init_search_backends` on the local
        LspBridge.  A failure of `init_search_backends` is logged on purpose
        so the event is still set and the remote side is not left waiting.
        """
        # remote server lsp-bridge process use this cient_socket to call elisp function from local Emacs.
        log_time(f"Client connect from {self.client_address[0]}:{self.client_address[1]}")

        for rpc in self.rpcs.values():
            rpc["result"] = None
            rpc["completion"].set()

        self.lsp_bridge.init_search_backends_complete_event.clear()

        threading.Thread(target=super().handle_client).start()

        try:
            self.lsp_bridge.init_search_backends()
            log_time("init_search_backends finish")
        except Exception:
            logger.exception("init_search_backends failed")
        finally:
            self.lsp_bridge.init_search_backends_complete_event.set()

    def handle_message(self, message):
        """Resolve the pending RPC that MESSAGE answers.

        Args:
            message: Either the literal "Connect" hello, which is dropped, or
                a dictionary with the request `timestamp` and its `result`.
        """
        if message == "Connect":
            log_time("Drop 'say hello' message from local Emacs.")
            return
        else:
            ts = message["timestamp"]
            self.rpcs[ts]["result"] = message["result"]
            self.rpcs[ts]["completion"].set()

    def call_remote_rpc(self, message):
        """Run one RPC on the remote side and return its result.

        Args:
            message: Request dictionary; a fresh `timestamp` key is added to
                match the answer to this call.

        Returns:
            The result sent back by the remote side, or None when the request
            could not be sent.

        The call blocks on a completion event until `handle_message` signals
        it, and the pending entry is removed from `self.rpcs` in both paths.
        """
        ts = time.monotonic_ns()
        cpl = threading.Event()
        try:
            message["timestamp"] = ts
            self.rpcs[ts] = { "msg": message, "completion": cpl }
            self.send_message(message)
        except Exception as e:
            logger.exception(e)
            del self.rpcs[ts]
            return None
        else:
            cpl.wait()
            result = self.rpcs[ts]["result"]
            del self.rpcs[ts]
            return result


class FileCommandServer(RemoteFileServer):
    """Run commands that the remote lsp-bridge sends to the local process.

    Requests either ask for an LSP request to be replayed against a remote
    file or for a lsp-bridge method to be called directly on the local side.
    """

    def __init__(self, host, port, lsp_bridge):
        """Start the command server bound to HOST:PORT.

        Args:
            host: Interface to bind, normally "0.0.0.0".
            port: TCP port of the command channel.
            lsp_bridge: The LspBridge whose event queue and methods are used
                to run the requests.
        """
        self.lsp_bridge = lsp_bridge
        super().__init__(host, port)

    def handle_client(self):
        """Wait for the search backends, then serve the connected client.

        Requests are only handled after `init_search_backends` has finished
        executing.
        """
        # Record server host when lsp-bridge running in remote server.
        # we wait for init_search_backends to finish execution
        # before start thread to handle remote request
        log_time("wait for init_search_backends to finsih execution")
        self.lsp_bridge.init_search_backends_complete_event.wait()

        super().handle_client()

    def handle_message(self, message):
        """Run the command carried by MESSAGE on the local side.

        Args:
            message: Request with `command` either `lsp_request`, which is
                queued as an LSP request for `path`, or `func_request`, which
                calls `method` on the LspBridge with `args`.
        """
        if message["command"] == "lsp_request":
            # Call LSP request.
            self.lsp_bridge.event_queue.put({
                "name": "action_func",
                "content": ("_{}".format(message["method"]), [message["path"]] + message["args"])
            })
        elif message["command"] == "func_request":
            # Call lsp-bridge normal function.
            getattr(self.lsp_bridge, message["method"])(*message["args"])


def save_ip_to_file(ip, filename):
    """Append IP to FILENAME unless it is already listed there.

    Args:
        ip: Address to remember, one per line.
        filename: File that already exists and holds the known addresses.
    """
    with open(filename, 'r') as f:
        existing_ips = f.read().splitlines()

    if ip in existing_ips:
        return

    existing_ips.append(ip)

    with open(filename, 'w') as f:
        f.write('\n'.join(existing_ips))


def get_container_local_ip(container_name):
    """Return the local IP address of CONTAINER_NAME, or None.

    Args:
        container_name: Docker container whose first network address is
            looked up with `docker inspect`.

    Returns:
        The container IP address, or None when docker inspect fails; the
        failure is reported to Emacs and not raised.
    """
    try:
        command = "docker inspect -f '{{range.NetworkSettings.Networks}}{{.IPAddress}}{{end}}' " + container_name
        result = subprocess.check_output(command, shell=True, text=True).strip()
        return result
    except subprocess.CalledProcessError:
        message_emacs(f"get_container_local_ip error: {traceback.format_exc()}")
        return None


def save_ip(ip):
    """Persist IP in the remote_file/ip.txt file of the user's Emacs directory.

    Args:
        ip: Remote connection info to remember; the file is created when it
            does not exist yet.
    """
    user_emacs_dir = get_emacs_func_result("get-user-emacs-directory")
    ip_file = os.path.join(user_emacs_dir, "lsp-bridge", "remote_file", "ip.txt")
    touch(ip_file)

    save_ip_to_file(ip, ip_file)
