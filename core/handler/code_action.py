from core.handler import Handler
from core.utils import *


class CodeAction(Handler):
    """Collect the code actions a server offers for a range and its diagnostics.

    The reply is pushed on the file action with the originating server name and
    action kind so that several servers can each contribute actions.
    """

    name = "code_action"
    method = "textDocument/codeAction"
    cancel_on_change = True
    provider = "code_action_provider"
    provider_message = "Current server not support code action."

    def process_request(self, lsp_server_name, diagnostics, range_start, range_end, action_kind) -> dict:
        """Return the params for the range, its DIAGNOSTICS and ACTION_KIND.

        LSP_SERVER_NAME and ACTION_KIND are remembered on `self` so that
        `process_response` can tag the actions with their origin; when
        ACTION_KIND is a string it is also sent as the `only` filter.
        """
        self.action_kind = action_kind
        self.lsp_server_name = lsp_server_name

        range = {
            "start": range_start,
            "end": range_end
        }

        if isinstance(action_kind, str):
            context = {
                "diagnostics": diagnostics,
                "only": [action_kind]
            }
        else:
            context = {
                "diagnostics": diagnostics
            }

        return dict(range=range, context=context)

    def process_response(self, response) -> None:
        """Push the actions in RESPONSE on the file action for Emacs to pick."""
        remote_connection_info = get_remote_connection_info()
        if remote_connection_info != "":
            for item in response:
                convert_workspace_edit_path_to_tramped_path(item["edit"], remote_connection_info)
        self.file_action.push_code_actions(response, self.lsp_server_name, self.action_kind)
