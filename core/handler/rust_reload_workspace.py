from core.handler import Handler
from core.utils import *

class RustReloadWorkspace(Handler):
    """Ask rust-analyzer to reload its workspace.

    It is driven by `lsp-bridge-rust-reload-workspace`; the notification
    carries no params and its empty reply is ignored.
    """

    name = "rust_reload_workspace"
    method = "rust-analyzer/reloadWorkspace"
    cancel_on_change = False
    send_document_uri = False

    def process_request(self) -> dict:
        """Return no params for the reloadWorkspace notification."""
        return None

    def process_response(self, response) -> None:
        """Ignore RESPONSE; the notification has no useful result."""
        pass
