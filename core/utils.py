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
import logging
import os
import pathlib
import platform
import queue
import re
import subprocess
import sys
from threading import Thread
from typing import Optional, List
from urllib.parse import urlparse

import sexpdata
from epc.client import EPCClient
from functools import wraps
import threading

try:
    import orjson as json_parser
except Exception:
    import json as json_parser

KIND_MAP = ["", "Text", "Method", "Function", "Constructor", "Field",
            "Variable", "Class", "Interface", "Module", "Property",
            "Unit", "Value", "Enum", "Keyword", "Snippet", "Color",
            "File", "Reference", "Folder", "EnumMember", "Constant",
            "Struct", "Event", "Operator", "TypeParameter"]

SYMBOL_MAP = ["", "File", "Module", "Namespace", "Package",
              "Class", "Method", "Property", "Field", "Constructor",
              "Enum", "Interface", "Function", "Variable", "Constant",
              "String", "Number", "Boolean", "Array", "Object",
              "Key", "Null", "EnumMember", "Struct", "Event",
              "Operator", "TypeParameter"]

epc_client: Optional[EPCClient] = None

# for test purpose
test_interceptor = None

# initialize logging, default to STDERR and INFO level
logger = logging.getLogger("lsp-bridge")
logger.setLevel(logging.INFO)
logger.addHandler(logging.StreamHandler())


def init_epc_client(emacs_server_port):
    """Create the EPC client that talks to Emacs on EMACS_SERVER_PORT.

    The client is stored in the module global `epc_client` and a refused
    connection is only logged.
    """
    global epc_client

    if epc_client is None:
        try:
            epc_client = EPCClient(("127.0.0.1", emacs_server_port), log_traceback=True)
        except ConnectionRefusedError:
            import traceback
            logger.error(traceback.format_exc())


def close_epc_client():
    """Close the module EPC client when one has been created."""
    if epc_client is not None:
        epc_client.close()


def handle_arg_types(arg):
    """Return ARG wrapped as a quoted sexp for sending to Emacs.

    A string starting with a quote becomes a symbol, everything else stays a
    quoted value.
    """
    if isinstance(arg, str) and arg.startswith("'"):
        arg = sexpdata.Symbol(arg.partition("'")[2])

    return sexpdata.Quoted(arg)

running_in_server = False
def set_running_in_server():
    """Mark the module as running inside the LSP bridge server process."""
    global running_in_server

    running_in_server = True

def is_running_in_server():
    """Return non-nil when running inside the LSP bridge server process."""
    global running_in_server

    return running_in_server

is_merge_emacs_exec_path = False
def merge_emacs_exec_path():
    """Append Emacs `exec-path` entries to the process PATH once."""
    global is_merge_emacs_exec_path

    # Only merge once.
    if not is_merge_emacs_exec_path:
        paths = os.environ["PATH"].split(":")
        [emacs_paths, ] = get_emacs_vars(["exec-path"])
        all_paths = [i for i in paths + emacs_paths if not isinstance(i,list)]
        new_paths = list(dict.fromkeys(all_paths))

        os.environ["PATH"] = ":".join(new_paths)

        is_merge_emacs_exec_path = True

lsp_bridge_server = None
def set_lsp_bridge_server(bridge):
    """Store BRIDGE as the module level server used by the helpers below."""
    global lsp_bridge_server
    lsp_bridge_server = bridge

def get_lsp_file_host():
    """Return the remote host the file commands came from, or an empty string."""
    global lsp_bridge_server
    if lsp_bridge_server and lsp_bridge_server.file_command_server:
        return lsp_bridge_server.file_command_server.client_address[0]
    else:
        return ""

def get_buffer_content(filename, buffer_name):
    """Return the content of FILENAME, asking Emacs for BUFFER_NAME as fallback."""
    global lsp_bridge_server

    content = ""
    if lsp_bridge_server and lsp_bridge_server.file_server:
        content = lsp_bridge_server.file_server.get_file_content(filename)
    if not content:
        # Fallback to Emacs buffer for local files (e.g. org-babel)
        content = get_emacs_func_result('get-buffer-content', buffer_name)
    return content

def get_file_content_from_file_server(filename):
    """Return the content of FILENAME from the file server, or an empty string."""
    global lsp_bridge_server

    if lsp_bridge_server and lsp_bridge_server.file_server:
        return lsp_bridge_server.file_server.get_file_content(filename)
    else:
        return ""

def get_current_line():
    """Return the current line of the active Emacs buffer."""
    return get_emacs_func_result('get-current-line')

def get_ssh_password(user, host, port):
    """Return the SSH password Emacs holds for USER at HOST on PORT."""
    return get_emacs_func_result('get-ssh-password', user, host, port)

remote_connection_info = ""
def set_remote_connection_info(remote_info):
    """Store the tramp method used by the current REMOTE_INFO connection."""
    global remote_connection_info
    remote_connection_info = remote_info

def get_remote_connection_info():
    """Return the tramp method of the current remote connection."""
    global remote_connection_info
    return remote_connection_info

def convert_workspace_edit_path_to_tramped_path(edit, remote_connection_info):
    """Convert documentUris in a WorkspaceEdit instance from local to remote(tramp).

    ex. 'file://...' --> 'file://ssh:...

    About WorkspaceEdit interfeface:
    https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspaceEdit
    """
    if "documentChanges" in edit:
        # documentChanges's item can be one of (TextDocumentEdit, CreateFile, DeleteFile, RenameFile)
        for change in edit["documentChanges"]:
            if change.get("textDocument", {}).get("uri") is not None:
                # TextDocumentEdit
                change["textDocument"]["uri"] = local_path_to_tramp_path(change["textDocument"]["uri"], remote_connection_info)
            elif "uri" in change:
                # CreateFile | DeleteFile
                change["uri"] = local_path_to_tramp_path(change["uri"], remote_connection_info)
            elif "oldUri" in change:
                # RenameFile
                change["oldUri"] = local_path_to_tramp_path(change["oldUri"], remote_connection_info)
                change["newUri"] = local_path_to_tramp_path(change["newUri"], remote_connection_info)
    elif "changes" in edit:
        changes = edit["changes"]
        new_changes = {}
        for file in changes.keys():
            tramp_file = local_path_to_tramp_path(file, remote_connection_info)
            new_changes[tramp_file] = changes[file]
        edit["changes"] = new_changes

def local_path_to_tramp_path(path, tramp_method):
    """Convert path in DocumentUri format to tramp format."""
    tramp_path = path.replace("file://", "file://" + tramp_method)
    return tramp_path

def eval_in_emacs(method_name, *args):
    """Call the elisp METHOD_NAME synchronously with ARGS.

    The arguments are encoded as sexps and sent through the file command
    server when one exists, otherwise through the EPC client.
    """
    global lsp_bridge_server

    if test_interceptor:  # for test purpose, record all eval_in_emacs calls
        test_interceptor(method_name, args)

    args = [sexpdata.Symbol(method_name)] + list(map(handle_arg_types, args))    # type: ignore
    sexp = sexpdata.dumps(args)

    logger.debug("Eval in Emacs: %s", sexp)

    # Call eval-in-emacs elisp function.
    if lsp_bridge_server and lsp_bridge_server.file_command_server:
        lsp_bridge_server.file_command_server.send_message({
            "command": "eval-in-emacs",
            "sexp": [sexp]
        })
    else:
        eval_sexps_in_emacs([sexp])

# The channels the remote file machinery talks on; shared by the modules
# that send on them, so they live with the other shared definitions.
REMOTE_FILE_SYNC_CHANNEL = 9999
REMOTE_FILE_COMMAND_CHANNEL = 9998
REMOTE_FILE_ELISP_CHANNEL = 9997


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


def message_emacs(message: str):
    """Message to Emacs with prefix."""
    eval_in_emacs("message", "[LSP-Bridge] " + message)


def epc_arg_transformer(arg):
    """Transform an elisp object into its Python equivalent.

    1                          => 1
    "string"                   => "string"
    (list :a 1 :b 2)           => {"a": 1, "b": 2}
    (list :a 1 :b (list :c 2)) => {"a": 1, "b": {"c": 2}}
    (list 1 2 3)               => [1 2 3]
    (list 1 2 (list 3 4))      => [1 2 [3 4]]
    """
    if not isinstance(arg, list):
        return arg

    # NOTE: Empty list elisp can be treated as both empty python dict/list
    # Convert empty elisp list to empty python dict due to compatibility.

    # check if we can tranform arg to python dict instance
    type_dict_p = len(arg) % 2 == 0
    if type_dict_p:
        for v in arg[::2]:
            if not isinstance(v, sexpdata.Symbol) or not v.value().startswith(":"):
                type_dict_p = False
                break

    if type_dict_p:
        # transform [Symbol(":a"), 1, Symbol(":b"), 2] to dict(a=1, b=2)
        ret = dict()
        for i in range(0, len(arg), 2):
            ret[arg[i].value()[1:]] = epc_arg_transformer(arg[i + 1])
        return ret
    else:
        return list(map(epc_arg_transformer, arg))


def convert_emacs_bool(symbol_value, symbol_is_boolean):
    """Return SYMBOL_VALUE converted to a bool when SYMBOL_IS_BOOLEAN is "t"."""
    if symbol_is_boolean == "t":
        return symbol_value is True
    else:
        return symbol_value


def get_emacs_vars(args):
    """Return the values of the Emacs variables named in ARGS."""
    global lsp_bridge_server

    if lsp_bridge_server and lsp_bridge_server.file_elisp_server:
        return lsp_bridge_server.file_elisp_server.call_remote_rpc({
            "command": "get_emacs_vars",
            "args": args
        })
    else:
        results = epc_client.call_sync("get-emacs-vars", args)
        return list(map(lambda result: convert_emacs_bool(result[0], result[1]) if result != [] else False, results))

def get_emacs_func_result(method_name, *args):
    """Call eval-in-emacs elisp function synchronously and return the result."""
    global lsp_bridge_server

    if lsp_bridge_server and lsp_bridge_server.file_elisp_server:
        return lsp_bridge_server.file_elisp_server.call_remote_rpc({
            "command": "get_emacs_func_result",
            "method": method_name,
            "args": args
        })
    else:
        result = epc_client.call_sync(method_name, args)    # type: ignore
        return result

def get_command_result(command_string, cwd):
    """Run COMMAND_STRING in CWD and return its stripped output.

    The stdout is returned when the command succeeds, otherwise its stderr.
    """
    import subprocess

    major, minor = sys.version_info[:2]

    if major >= 3 and minor >= 7:
        process = subprocess.Popen(command_string, cwd=cwd, shell=True, text=True,
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                   encoding="utf-8", errors="replace")
    else:
        process = subprocess.Popen(command_string, cwd=cwd, shell=True, universal_newlines=True,
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                   encoding="utf-8", errors="replace")

    ret = process.wait()
    return "".join((process.stdout if ret == 0 else process.stderr).readlines()).strip()


def generate_request_id():
    """Return a new random JSON-RPC request id."""
    import random
    return abs(random.getrandbits(16))


# modified from Lib/pathlib.py
def _make_uri_win32(path):
    """Return the file URI for the Windows PATH, as vscode encodes it."""
    from urllib.parse import quote_from_bytes as urlquote_from_bytes
    # Under Windows, file URIs use the UTF-8 encoding.
    drive = path.drive
    if len(drive) == 2 and drive[1] == ':':
        # It's a path on a local drive => 'file:///c:/a/b'
        rest = path.as_posix()[2:].lstrip('/')
        return 'file:///%s%%3A/%s' % (
            drive[0], urlquote_from_bytes(rest.encode('utf-8')))
    else:
        # It's a path on a network drive => 'file://host/share/a/b'
        return 'file:' + urlquote_from_bytes(path.as_posix().encode('utf-8'))

# From jedi/api/helpers.py
def _fuzzy_match(string, like_name):
    """Return whether LIKE_NAME appears in order inside STRING."""
    if len(like_name) <= 1:
        return like_name in string
    pos = string.find(like_name[0])
    if pos >= 0:
        return _fuzzy_match(string[pos + 1:], like_name[1:])
    return False

def _start_match(string, like_name):
    """Return non-nil when STRING starts with LIKE_NAME."""
    return string.startswith(like_name)

def _substring_match(string, like_name):
    """Return non-nil when LIKE_NAME appears inside STRING."""
    if len(like_name) <= 1:
        return like_name in string
    return string.find(like_name) >= 0

def string_match(string, like_name, match_mode="prefix", case_mode="ignore"):
    """Return non-nil when STRING matches LIKE_NAME for completion.

    MATCH_MODE is "prefix", "substring", "prefixCaseSensitive" or anything else
    for fuzzy matching, and CASE_MODE is "smart", "ignore" or "sensitive".
    """
    # For compatibility reasons: match_mode can be prefixCaseSensitiv
    if match_mode == "prefixCaseSensitive":
        case_mode = "sensitive"
        match_mode = "prefix"

    match case_mode:
        case "smart":
            if not any(c.isupper() for c in like_name):
                string = string.lower()
        case "ignore":
            string = string.lower()
            like_name = like_name.lower()

    match match_mode:
        case "prefix":
            return _start_match(string, like_name)
        case "substring":
            return _substring_match(string, like_name)
        case _: # fuzzy match
            return _fuzzy_match(string, like_name)

def path_to_uri(path):
    """Return the file URI of PATH, encoded for Windows when needed."""
    path = pathlib.Path(path)
    if get_os_name() != "windows":
        uri = path.as_uri()
    else:
        if not path.is_absolute():
            raise ValueError("relative path can't be expressed as a file URI")
        # encode uri to 'file:///c%3A/project/xxx.js' like vscode does
        uri = _make_uri_win32(path)
    return uri


def uri_to_path(uri):
    """Return the local file path encoded in URI."""
    from urllib.parse import unquote
    # parse first, '#' may be part of filepath(encoded)
    parsed = urlparse(uri)
    # for example, ts-ls return 'file:///c%3A/lib/ref.js'
    path = unquote(parsed.path)
    if sys.platform == "win32":
        path = path[1:]
    return path


def path_as_key(path):
    """Return PATH normalised into the key used by the path dictionaries."""
    key = path
    # NOTE: (buffer-file-name) return "d:/Case/a.go", gopls return "file:///D:/Case/a.go"
    if sys.platform == "win32":
        path = pathlib.Path(path).as_posix()
        key = path.lower()
    return key


def add_to_path_dict(path_dict, filepath, value):
    """Store VALUE in PATH_DICT under the key of FILEPATH."""
    path_dict[path_as_key(filepath)] = value


def is_in_path_dict(path_dict, path):
    """Return non-nil when the key of PATH is present in PATH_DICT."""
    path_key = path_as_key(path)
    return path_key in path_dict


def remove_from_path_dict(path_dict, path):
    """Delete the entry of PATH from PATH_DICT."""
    del path_dict[path_as_key(path)]


def get_from_path_dict(path_dict, filepath):
    """Return the value PATH_DICT holds for FILEPATH."""
    return path_dict[path_as_key(filepath)]


def get_project_path(filepath):
    """Return the project root that contains FILEPATH.

    Emacs decides the root first; git is asked when Emacs returns nothing, and
    FILEPATH itself is used when it is not inside a git work tree.
    """
    project_path = get_emacs_func_result("get-project-path", filepath)

    if isinstance(project_path, str):
        return project_path
    else:
        import os
        dir_path = os.path.dirname(filepath)
        if get_command_result("git rev-parse --is-inside-work-tree", dir_path) == "true":
            path_from_git = get_command_result("git rev-parse --show-toplevel", dir_path)
            if get_os_name() == "windows":
                path_parts = path_from_git.split("/")
                # if this is a Unix-style absolute path, which should be a Windows-style one
                if path_parts[0] == "/":
                    windows_path = path_parts[1] + ":/" + "/".join(path_parts[2:])
                    return windows_path
                else:
                    return path_from_git
            else:
                return path_from_git
        else:
            return filepath

def log_time(message):
    """Log MESSAGE with a timestamp prefix."""
    import datetime
    logger.info("\n--- [{}] {}".format(datetime.datetime.now().time(), message))

def log_time_debug(message):
    """Log MESSAGE with a timestamp prefix at debug level."""
    import datetime
    logger.debug("\n--- [{}] {}".format(datetime.datetime.now().time(), message))

def get_os_name():
    """Return the lower case name of the current platform."""
    return platform.system().lower()

def parse_json_content(content):
    """Return the Python object decoded from the JSON CONTENT."""
    return json_parser.loads(content)

def windows_get_env_value(var_name: str) -> str:
    """Read a Windows environment variable from os.environ and return its value."""
    if var_name in os.environ.keys():
        return os.environ[var_name]

def cmp(x, y):
    """Compare X and Y, returning -1, 0 or 1."""
    if x < y:
        return -1
    elif x > y:
        return 1
    else:
        return 0

def is_valid_ip(ip):
    """Return non-nil when IP is a dotted IPv4 address."""
    m = re.match(r"^(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})$", ip)
    return bool(m) and all(map(lambda n: 0 <= int(n) <= 255, m.groups()))

def is_valid_ip_path(ssh_path):
    """Check if SSH-PATH is a valid ssh path."""
    pattern = r"^/?(?:([a-z_][a-z0-9_\.-]*)@)?((?:[0-9]{1,3}\.){3}[0-9]{1,3})(?::(\d+))?:~?(.*)$"
    match = re.match(pattern, ssh_path)
    return match is not None

def split_ssh_path(ssh_path):
    """Split SSH-PATH into username, host, port and path."""
    pattern = r"^/?((?:([a-z_][a-z0-9_\.-]*)@)?((?:[0-9]{1,3}\.){3}[0-9]{1,3})(?::(\d+))?:?)(.*)$"
    match = re.match(pattern, ssh_path)
    if match:
        remote_info, username, host, port, path = match.groups()
        ssh_conf = {'hostname' : host}
        if username:
            ssh_conf['user'] = username
        if port:
            ssh_conf['port'] = port
        return (remote_info, host, ssh_conf, path)
    else:
        return None

def split_docker_path(docker_path):
    """Return (username, container_name, path) parsed from DOCKER_PATH."""
    # Pattern to extract username, container name, and path
    pattern = r"^/docker:(?P<username>[^@]+)@(?P<container_name>[^:]+):(?P<path>/.*)$"
    match = re.match(pattern, docker_path)
    if match:
        username = match.group('username')
        container_name = match.group('container_name')
        path = match.group('path')
        return (username, container_name, path)
    else:
        return None

def is_remote_path(filepath):
    """Return non-nil when FILEPATH points at an ssh or docker remote."""
    return filepath.startswith("/ssh:") or filepath.startswith("/docker:")

def eval_sexps_in_emacs(sexps: List[str]):
    """Send the SEXPS list to Emacs for evaluation."""
    if isinstance(sexps, str):
        sexps = [sexps]
        logger.warning(f"Fixed argument type. The sexp argument should be a list of sexps but got: {sexps}")
    epc_client.call("eval-in-emacs", sexps)

def string_to_base64(text):
    """Return TEXT encoded as a base64 string."""
    import base64

    base64_bytes = base64.b64encode(text.encode("utf-8"))
    base64_string = base64_bytes.decode("utf-8")

    return base64_string

def get_position(content, line, character):
    """Return the absolute offset of LINE and CHARACTER inside CONTENT."""
    lines = content.split('\n')
    position = sum(len(lines[i]) + 1 for i in range(line)) + character
    return position

def replace_template(arg, project_path=None):
    """Return ARG with its supported %...% placeholders expanded.

    PROJECT_PATH is accepted for callers but is not used by the current
    replacements.
    """
    if "%USER_EMACS_DIRECTORY%" in arg:
        if get_os_name() == "windows":
            user_emacs_dir = get_emacs_func_result("get-user-emacs-directory").replace("/", "\\")
        else:
            user_emacs_dir = get_emacs_func_result("get-user-emacs-directory")
        return arg.replace("%USER_EMACS_DIRECTORY%", user_emacs_dir)
    elif "$" in arg:
        return os.path.expandvars(arg)
    elif "%FILEHASH%" in arg:
        # pyright use `--cancellationReceive` option enable "background analyze" to improve completion performance.
        return arg.replace("%FILEHASH%", os.urandom(21).hex())
    elif "%USERPROFILE%" in arg:
        return arg.replace("%USERPROFILE%", repr(windows_get_env_value("USERPROFILE")).strip("'"))
    elif "%TSDK_PATH%" in arg:
        return arg.replace("%TSDK_PATH%", repr(get_emacs_func_result("get-user-tsdk-path")).strip("'"))
    else:
        return arg

def find_csharp_solution_file(directory):
    """Return the first .sln file found under DIRECTORY, or None."""
    for root, dirs, files in os.walk(directory):
        for file in files:
            if file.endswith(".sln"):
                return os.path.join(root, file)

    return None

def touch(path):
    """Create the empty file PATH, including its parent directories."""
    import os

    if not os.path.exists(path):
        basedir = os.path.dirname(path)

        if not os.path.exists(basedir):
            os.makedirs(basedir)

        with open(path, 'a'):
            os.utime(path)

def rebuild_content_from_diff(content, start_pos, end_pos, change_text):
    """Return CONTENT with the START_POS..END_POS range replaced by CHANGE_TEXT."""
    start_line = start_pos['line']
    start_char = start_pos['character']
    end_line = end_pos['line']
    end_char = end_pos['character']

    start_pos = get_position(content, start_line, start_char)
    end_pos = get_position(content, end_line, end_char)

    return content[:start_pos] + change_text + content[end_pos:]

def remove_duplicate_references(data):
    """Return the items of DATA without duplicated uri and range pairs."""
    seen = set()
    result = []
    for item in data:
        t_item = (item["uri"], tuple(item["range"]["start"].items()), tuple(item["range"]["end"].items()))
        if t_item not in seen:
            seen.add(t_item)
            result.append(item)
    return result

def get_value_from_path(data, path):
    """Retrieve a value from a nested dictionary based on the given path."""
    for key in path:
        if isinstance(data, dict) and key in data:
            data = data[key]
        else:
            return None
    return data

def get_nested_value(json_data, attribute_path):
    """Find the value of ATTRIBUTE_PATH inside JSON_DATA.

    Handles both a single path and a nested list path.
    """
    if isinstance(attribute_path[0], list):  # Handle nested list case
        for path in attribute_path:
            value = get_value_from_path(json_data, path)
            if value is not None:
                return value
        return None
    else:
        return get_value_from_path(json_data, attribute_path)

class MessageSender(Thread):
    """Base class for the threads that write messages to a process."""

    def __init__(self, process: subprocess.Popen):
        """Keep PROCESS and create the queue its messages are sent from."""
        super().__init__()

        self.process = process
        self.queue = queue.Queue()

    def send_request(self, message):
        """Queue MESSAGE for sending to the process."""
        self.queue.put(message)

class MessageReceiver(Thread):
    """Base class for the threads that read messages from a process."""

    def __init__(self, process: subprocess.Popen):
        """Keep PROCESS and create the queue its messages are read into."""
        super().__init__()

        self.process = process
        self.queue = queue.Queue()

    def get_message(self):
        """Block until the next message arrives and return it."""
        return self.queue.get(block=True)
