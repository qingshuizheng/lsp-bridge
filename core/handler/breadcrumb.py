from core.handler import Handler
from core.utils import *


class Breadcrumb(Handler):
    """Describe the nesting of symbols around the cursor.

    The `textDocument/documentSymbol` tree is walked to find the innermost
    symbol containing the cursor and its ancestors; the path is sent, outermost
    first, to `lsp-bridge-breadcrumb--callback` as
    (name, kind, position) triples.  A null, empty or flat
    SymbolInformation[] reply sends nil instead.
    """

    name = "breadcrumb"
    method = "textDocument/documentSymbol"
    send_document_uri = True

    def process_request(self, cursor) -> dict:
        """Remember CURSOR as `self.cursor` and return empty params."""
        self.cursor = cursor
        return dict()

    def process_response(self, response: dict) -> None:
        """Send the symbol path around `self.cursor` to Emacs."""
        fn = "lsp-bridge-breadcrumb--callback"
        filepath = self.file_action.filepath
        filehost = get_lsp_file_host()

        try:
            if (
                response is None
                or len(response) == 0
                # LSP returns SymbolInformation[], which is useless here.
                or "range" not in response[0]
            ):
                eval_in_emacs(fn, filepath, filehost, None)
                return

            parents = {}
            cursor = self.cursor
            cursor_symbol = None

            def traverse(parent, children):
                """Index PARENT for every CHILD and mark the one under cursor.

                Recurses into children, and sets `cursor_symbol` (from the
                enclosing scope) to the deepest symbol containing the cursor.
                """
                nonlocal cursor_symbol
                for child in children:
                    parents[id(child)] = parent
                    range = child["range"]
                    start = range["start"]
                    end = range["end"]

                    if (
                        (start["line"], start["character"])
                        <= (cursor["line"], cursor["character"])
                        <= (end["line"], end["character"])
                    ):
                        cursor_symbol = child

                    if "children" in child and child["children"]:
                        traverse(child, child["children"])

            traverse(None, response)

            if cursor_symbol is None:
                eval_in_emacs(fn, filepath, filehost, None)
                return

            segments = []
            current = cursor_symbol

            while True:
                if current is None:
                    break
                segments.append(
                    dict(
                        name=current["name"],
                        kind=SYMBOL_MAP[current["kind"]],
                        pos=current["range"]["start"],
                    )
                )
                current = parents[id(current)]

            eval_in_emacs(fn, filepath, filehost, reversed(segments))
        except Exception:
            import traceback

            logger.error(traceback.format_exc())
