from core.handler import Handler
from core.utils import *

class Rename(Handler):
    """Rename the symbol at the cursor through `textDocument/rename`.

    The workspace edit the server returns is rewritten to TRAMP paths and then
    applied by Emacs; a null reply only reports that no rename was found.
    """

    name = "rename"
    method = "textDocument/rename"

    def process_request(self, position, new_name) -> dict:
        """Return the params renaming the symbol at POSITION to NEW_NAME."""
        return dict(position=position, newName=new_name)

    def process_response(self, response: dict) -> None:
        """Apply the workspace edit in RESPONSE in Emacs."""
        if response is None:
            logger.info("No rename found.")
            message_emacs("No rename found")
            return

        remote_connection_info = get_remote_connection_info()
        logger.info(response)
        convert_workspace_edit_path_to_tramped_path(response, remote_connection_info)
        eval_in_emacs("lsp-bridge-workspace-apply-edit", response)

        message_emacs("Rename done.")
