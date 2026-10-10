from core.handler import Handler
from core.utils import *
from typing import Union

class RustExpandMacro(Handler):
    """Show the expanded form of the macro invocation at the cursor.

    The `rust-analyzer/expandMacro` reply is expected to carry `name` and
    `expansion`, which are passed to `lsp-bridge-rust-expand-macro--update`;
    anything else is only reported on stdout.
    """

    name = "rust_expand_macro"
    method = "rust-analyzer/expandMacro"
    cancel_on_change = True

    def process_request(self, position) -> dict:
        """Return the params for POSITION and remember it as `self.pos`."""
        self.pos = position
        return dict(position=position)

    def process_response(self, response: Union[dict, list]) -> None:
        """Send the macro name and expansion in RESPONSE to Emacs."""
        if response is None:
            print("Error: Received None response from rust-analyzer")
            return

        if "name" in response and "expansion" in response:
            eval_in_emacs("lsp-bridge-rust-expand-macro--update", response["name"], response["expansion"])
        else:
            print(f"Error: Response missing 'name' or 'expansion' keys. Response: {response}")
