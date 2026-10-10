from core.handler import Handler
from core.utils import *


class Formatting(Handler):
    """Format the whole buffer through `textDocument/formatting`.

    The returned text edits are sent to `lsp-bridge-format--update`; an empty
    reply only reports that nothing needed formatting.
    """

    name = "formatting"
    method = "textDocument/formatting"
    cancel_on_change = True
    provider = "code_format_provider"
    provider_message = "Current server not support code format."

    def process_request(self, tab_size) -> dict:
        """Return formatting options built from TAB_SIZE and the buffer indent."""
        options = {
            "tabSize": tab_size,
            "insertSpaces": self.file_action.insert_spaces,
            "trimTrailingWhitespace": True,
            "insertFinalNewline": False,
            "trimFinalNewlines": True,
        }

        return dict(options=options)

    def process_response(self, response) -> None:
        """Apply the edits in RESPONSE, or say nothing needed formatting."""
        if response and len(response) > 0:
            remote_connection_info = get_remote_connection_info()
            tramp_path = remote_connection_info + self.file_action.filepath
            eval_in_emacs("lsp-bridge-format--update", tramp_path, response)
        else:
            message_emacs("Nothing need format.")
