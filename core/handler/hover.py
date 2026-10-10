from core.handler import Handler
from core.utils import *


def make_code_block(language, string):
    """Return STRING wrapped in a Markdown fenced block tagged LANGUAGE."""
    return "```{language}\n{string}\n```".format(language=language, string=string)


class Hover(Handler):
    """Show the server's documentation for the symbol at the cursor.

    The reply's `contents` may be a string, a `{kind, value}` or
    `{language, value}` object, or a list of either; they are normalised into
    Markdown and sent to the popup or the documentation buffer depending on
    SHOW_STYLE.  rust-analyzer is the only server that gets a position range
    instead of a single position.
    """

    name = "hover"
    method = "textDocument/hover"

    def process_request(self, start, end, show_style) -> dict:
        """Return hover params for START..END, remembering SHOW_STYLE.

        Only rust-analyzer accepts the range form (a non-standard extension);
        every other server, and an empty selection, gets START as the position.
        """
        lsp_server_name = self.file_action.get_match_lsp_servers("hover")[0].server_info['name']
        self.show_style = show_style

        # rust-analyzer support range hover (not in LSP standard)
        if start == end or lsp_server_name != "rust-analyzer":
            return dict(position=start)
        else:
            range = {"start": start, "end": end}
            return dict(position=range)

    def parse_hover_contents(self, contents, render_strings):
        """Append CONTENTS rendered as Markdown to RENDER_STRINGS.

        Returns RENDER_STRINGS joined by newlines; the method recurses for
        lists and for the LSP `MarkedString`/`MarkupContent` shapes.
        """
        if isinstance(contents, str):
            if contents.startswith("```"):
                render_strings.append(contents)
            else:
                render_strings.append(make_code_block("text", contents))
        elif isinstance(contents, dict):
            if "kind" in contents:
                # Some language servers will return plaintext as the kind with the markdown format as value, such as erlang_ls
                if contents["kind"] == "markdown" or contents["kind"] == "plaintext":
                    render_strings.append(contents["value"])
                else:
                    lsp_server = self.file_action.get_match_lsp_servers("hover")[0]
                    render_strings.append(make_code_block(
                        lsp_server.get_language_id(self.file_action),
                        contents["value"]
                    ))
            elif "language" in contents:
                render_strings.append(make_code_block(contents["language"], contents["value"]))
        elif isinstance(contents, list):
            language = ""
            for item in contents:
                if isinstance(item, dict):
                    language = item["language"]
                    self.parse_hover_contents(item, render_strings)
                if isinstance(item, str):
                    if item != "":
                        if language == "java":
                            render_strings.append(item)
                        else:
                            self.parse_hover_contents(item, render_strings)
        return "\n".join(render_strings)

    def process_response(self, response: dict) -> None:
        """Render the `contents` of RESPONSE in Emacs."""
        if response is None or "contents" not in response or len(response["contents"]) == 0:
            message_emacs("No documentation available.")
            return

        contents = response["contents"]
        render_string = self.parse_hover_contents(contents, [])

        if self.show_style == "popup":
            callback = "lsp-bridge-popup-documentation--callback"
        else:
            callback = "lsp-bridge-show-documentation--callback"
        eval_in_emacs(callback, render_string)
