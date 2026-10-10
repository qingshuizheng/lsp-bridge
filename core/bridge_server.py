"""The language server lifecycle of the bridge process.

Mixed into `LspBridge`: picking the servers of a file, checking that their
commands exist, loading a single server info, creating the language servers
and turning them off again.
"""

import os
import shutil
import json
from pathlib import Path
from core.fileaction import create_file_action_with_single_server, FILE_ACTION_DICT, LSP_SERVER_DICT
from core.lspserver import LspServer
from core.utils import *
from core.handler import *


class ServerBridgeMixin:
    """The language server lifecycle of the bridge process.

    Mixed into `LspBridge'; the methods use the attributes that
    `LspBridge.__init__' sets up.
    """

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

    server_dir = REPO_ROOT / lang_server_dir
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
