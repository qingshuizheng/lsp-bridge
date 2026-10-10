from core.handler import Handler
from core.utils import *

class DocumentSymbol(Handler):
    """Record the enclosing definition under the cursor in Emacs.

    It walks the `textDocument/documentSymbol` tree and joins the names of
    every symbol whose range contains the requested position, then hands the
    dotted string to `lsp-bridge-symbols--record-current-defun`.
    """

    name = "document_symbol"
    method = "textDocument/documentSymbol"
    send_document_uri = True
    position = None

    def process_request(self, position) -> dict:
        """Remember POSITION as `self.position` and return empty params."""
        self.position = position
        return dict()

    def process_response(self, response: dict) -> None:
        """Send the innermost symbol path around `self.position` to Emacs."""
        if response is not None:
            try:
                symbols = []
                current_defun = ''
                while response:
                    symbol = response.pop()
                    if "range" in symbol:
                        range = symbol["range"]
                    else:
                        range = symbol['location']["range"]
                    if range['start']['line'] < self.position['line'] < range['end']['line'] or\
                       (range['start']['line'] == self.position['line'] and range['start']['character'] <= self.position['character']) or\
                       (range['end']['line'] == self.position['line'] and range['end']['character'] >= self.position['character']):
                        if len(current_defun):
                            current_defun += '.'
                        current_defun += symbol['name']
                        symbols.append(symbol)
                    if "children" in symbol and symbol["children"]:
                        response.extend(reversed(symbol["children"]))
                eval_in_emacs("lsp-bridge-symbols--record-current-defun", current_defun)
            except Exception:
                import traceback
                logger.error(traceback.format_exc())
