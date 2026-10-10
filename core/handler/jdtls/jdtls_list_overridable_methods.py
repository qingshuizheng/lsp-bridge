from core.handler import Handler
from core.utils import *
import json

class JdtlsListOverridableMethods(Handler):
    """List the overridable methods of a Java type in Emacs.

    The request body doubles as the context that has to be sent back with the
    chosen method, so it is kept until the reply arrives; the methods and that
    context are then passed as JSON to
    `lsp-bridge-jdtls-add-overridable-methods`.
    """

    name = "jdtls_list_overridable_methods"
    method = "java/listOverridableMethods"
    cancel_on_change = True
    send_document_uri = False
    context = None

    def process_request(self, arguments) -> dict:
        """Return ARGUMENTS as the request body and remember it as `context`."""
        self.context = arguments
        return arguments

    def process_response(self, response) -> None:
        """Show the overridable methods in RESPONSE, with `self.context`."""
        if response is not None and len(response) > 0:
            resp = {
                "response": response,
                "context": self.context
            }
            eval_in_emacs("lsp-bridge-jdtls-add-overridable-methods", json.dumps(resp))
            self.context = None
        else:
            message_emacs("No Overridable methods found.")
