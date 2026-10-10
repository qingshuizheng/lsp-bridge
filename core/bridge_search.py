"""The search backends, ctags and copilot of the bridge process.

Mixed into `LspBridge`: building the EPC entries of every search backend and
of the completion sources that are not a language server (elisp symbols,
snippets, paths, ctags, copilot)."""

import queue
import threading
from core.search_file_words import SearchFileWords
from core.search_list import SearchList
from core.search_paths import SearchPaths
from core.copilot import Copilot
from core.utils import *
from core.handler import *
from core.ctags import Ctags


class SearchBridgeMixin:
    """The search backends, ctags and copilot of the bridge process.

    Mixed into `LspBridge'; the methods use the attributes that
    `LspBridge.__init__' sets up.
    """

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
