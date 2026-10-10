from core.handler import Handler
from core.utils import *


class IMenu(Handler):
    """Feed the `imenu` index of a file from the LSP symbols.

    The `textDocument/documentSymbol` reply is grouped by symbol kind and
    translated to the names in `SYMBOL_MAP` before being passed to
    `lsp-bridge--imenu-show` together with the file path and host.
    """

    name = "imenu"
    method = "textDocument/documentSymbol"
    send_document_uri = True

    def process_request(self) -> dict:
        """Return empty params; the document is chosen by the file action."""
        return dict()

    def process_response(self, response: dict) -> None:
        """Group RESPONSE by kind and show it in Emacs' imenu."""
        if response is not None:
            try:
                grouped_data = {}

                for item in response:
                    kind = item['kind']
                    if "range" not in item:
                        item["range"] = item['location']["range"]

                    if kind not in grouped_data:
                        grouped_data[kind] = []
                    grouped_data[kind].append(item)

                new_response = []
                for kind, items in grouped_data.items():
                    new_response.append({'name': SYMBOL_MAP[kind], 'children': items})

                eval_in_emacs("lsp-bridge--imenu-show", self.file_action.filepath, get_lsp_file_host(), new_response)
            except Exception:
                import traceback
                logger.error(traceback.format_exc())
