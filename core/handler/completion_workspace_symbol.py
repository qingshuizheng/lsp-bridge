from core.handler import Handler
from core.utils import *
from functools import cmp_to_key


class CompletionWorkspaceSymbol(Handler):
    """Offer workspace symbols as completion candidates.

    The `workspace/symbol` reply is turned into acm items labelled
    `NAME [kind]`, sorted so that labels starting with the typed prefix come
    first and then by length, truncated to
    `completion_workspace_symbol_items_limit`, and finally recorded by
    `lsp-bridge-completion-workspace-symbol--record-items`.
    """

    name = "completion_workspace_symbol"
    method = "workspace/symbol"
    provider = "workspace_symbol_provider"

    def process_request(self, lsp_server, query) -> dict:
        """Return the params for QUERY and remember its server and prefix.

        LSP_SERVER is remembered as `self.lsp_server` and its name as
        `self.lsp_server_name`; the raw QUERY is kept as `self.prefix` because
        the whitespace is stripped from the query that is actually sent.
        """
        self.prefix = query
        self.lsp_server = lsp_server
        self.lsp_server_name = self.lsp_server.server_info["name"]
        query = ''.join(query.split())
        return dict(query=query)

    def compare_candidates(self, x, y):
        """Order X before Y if it matches the prefix, then if it is shorter."""
        prefix = self.prefix.lower()
        x_label, y_label = x["label"].lower(), y["label"].lower()
        x_include_prefix, y_include_prefix = x_label.startswith(prefix), y_label.startswith(prefix)

        if x_include_prefix != y_include_prefix:
            return -1 if x_include_prefix else 1

        return -1 if len(x_label) < len(y_label) else (1 if len(x_label) > len(y_label) else 0)

    def process_response(self, response: dict) -> None:
        """Record the sorted, truncated symbols in RESPONSE for Emacs."""
        if response is not None:
            completion_symbols = []

            for item in response:
                symbol_name = item["name"]
                symbol_kind = SYMBOL_MAP[item.get("kind", 0)].lower()
                symbol_display_name = "{} [{}]".format(symbol_name, symbol_kind)
                symbol = {
                    "key": symbol_name,
                    "icon": "workspace-symbol",
                    "label": symbol_name,
                    "displayLabel": symbol_display_name,
                    "server": self.lsp_server_name,
                    "backend": "lsp-workspace-symbol"
                }

                completion_symbols.append(symbol)

            completion_symbols = sorted(completion_symbols, key=cmp_to_key(self.compare_candidates))

            # Avoid returning too many items to cause Emacs to do GC operation.
            completion_symbols = completion_symbols[:min(len(completion_symbols), self.file_action.completion_workspace_symbol_items_limit)]

            if len(completion_symbols) > 0:
                eval_in_emacs("lsp-bridge-completion-workspace-symbol--record-items",
                              self.file_action.filepath,
                              get_lsp_file_host(),
                              completion_symbols,
                              self.lsp_server_name,
                              self.file_action.get_lsp_server_names())
