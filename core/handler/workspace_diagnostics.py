from core.handler import Handler
from core.utils import *

class WorkspaceDiagnostics(Handler):
    """Request the server's diagnostics for every file in the workspace.

    The request carries an empty list of previous result ids, and the reply is
    currently only printed for debugging.
    """

    name = "workspace_diagnostics"
    method = "workspace/diagnostic"
    cancel_on_change = False
    send_document_uri = False

    def process_request(self) -> dict:
        """Return params with no previously reported diagnostic ids."""
        return dict(previousResultIds=[])

    def process_response(self, response: dict) -> None:
        """Print RESPONSE for debugging; it is not shown in Emacs."""
        print("****** ", response)
