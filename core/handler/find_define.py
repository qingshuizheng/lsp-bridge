from core.handler import Handler
from core.handler.solidity import find_define_response
from core.utils import *
from typing import Union


class FindDefine(Handler):
    """Jump to the definition of the symbol at the cursor.

    The single location returned by `textDocument/definition` is handed to
    `find_define_response`, which knows how to resolve the virtual URIs of
    Java, C#, Deno and Solidity servers before calling
    `lsp-bridge-define--jump`.
    """

    name = "find_define"
    method = "textDocument/definition"
    cancel_on_change = True

    def process_request(self, position) -> dict:
        """Return the params for POSITION and remember it as `self.pos`."""
        self.pos = position
        return dict(position=position)

    def process_response(self, response: Union[dict, list]) -> None:
        """Jump to the location in RESPONSE using the `find_define` rules."""
        find_define_response(self, response, "lsp-bridge-define--jump")
