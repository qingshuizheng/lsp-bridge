from core.handler import Handler
from core.utils import *


class PrepareRename(Handler):
    """Highlight the placeholder a rename would replace.

    Servers may answer with either a bare range or a `{"range": ...}` object
    (gopls); after unwrapping, the range is sent to
    `lsp-bridge-rename--highlight`.
    """

    name = "prepare_rename"
    method = "textDocument/prepareRename"
    provider = "rename_prepare_provider"
    provider_message = "Current server not support rename."

    def process_request(self, position) -> dict:
        """Return the params for POSITION."""
        return dict(position=position)

    def process_response(self, response: dict) -> None:
        """Highlight the range in RESPONSE that a rename would rewrite."""
        # compatible for gopls
        if "range" in response:
            response = response["range"]

        remote_connection_info = get_remote_connection_info()
        tramp_path = remote_connection_info + self.file_action.filepath
        eval_in_emacs("lsp-bridge-rename--highlight", tramp_path, get_lsp_file_host(), response["start"], response["end"])
