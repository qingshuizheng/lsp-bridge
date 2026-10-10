from core.handler import Handler
from core.utils import *


class JdtlsAddOverridableMethods(Handler):
    """Add the methods the user picked to a Java type.

    The reply is a workspace edit, which is applied by Emacs through
    `lsp-bridge-workspace-apply-edit`.
    """

    name = "jdtls_add_overridable_methods"
    method = "java/addOverridableMethods"
    cancel_on_change = True
    send_document_uri = False

    def process_request(self, params) -> dict:
        """Return the JSON PARAMS decoded into the request body."""
        return parse_json_content(params)

    def process_response(self, response) -> None:
        """Apply the workspace edit in RESPONSE to the open buffers."""
        if response is not None and len(response) > 0:
            eval_in_emacs("lsp-bridge-workspace-apply-edit", response)
