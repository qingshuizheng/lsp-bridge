"""The remote and tramp half of the bridge process.

Mixed into `LspBridge`: the socket client, the file client for a remote host
or a container, the message senders and receivers, the tramp information sync
and the file operations that only make sense for a remote buffer."""

import os
import queue
import traceback
import time
import socket
from pathlib import Path
from core.utils import *
from core.handler import *
from core.remote_file import RemoteFileClient, DockerFileClient, FileSyncServer, FileElispServer, FileCommandServer, ContainerConnectionException, save_ip, get_container_local_ip


class RemoteBridgeMixin:
    """The remote and tramp half of the bridge process.

    Mixed into `LspBridge'; the methods use the attributes that
    `LspBridge.__init__' sets up.
    """

    def message_hostnames(self):
        """Send the known remote host names to Emacs as a message.

        Exported over EPC; called by the Emacs command
        ``lsp-bridge-tramp-show-hostnames``.  The names are shown through
        ``message_emacs`` rather than returned.
        """
        message_emacs(f"host_names:{self.host_names}")

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
        except Exception:
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
