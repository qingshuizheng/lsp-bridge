
from core.handler import Handler
from core.handler.decompile import create_decompile_external_file
from core.utils import *


class DenoUriResolver(Handler):
    """Resolve Deno's `deno:` virtual URIs to real source files.

    The virtual document is fetched with `deno/virtualTextDocument`, written to
    a decompiled temp file by `create_decompile_external_file` and then opened
    at the recorded position with the original define handler.
    """

    name = "deno_uri_resolver"
    method = "deno/virtualTextDocument"
    cancel_on_change = True
    send_document_uri = False

    def __init__(self, *args, **kwargs):
        """Initialize the resolver with no pending URI or jump position."""
        super().__init__(*args, **kwargs)
        self.external_file_link = ""
        self.start_pos = None

    def process_request(self, uri, start_pos, define_jump_handler) -> dict:
        """Return params for URI and remember where the jump should land.

        START_POS and DEFINE_JUMP_HANDLER are kept for `process_response`.
        """
        self.start_pos = start_pos
        self.external_file_link = uri
        self.define_jump_handler = define_jump_handler
        return dict(textDocument={"uri": uri})

    def process_response(self, response):
        """Write the virtual document in RESPONSE to a file and jump into it."""
        if response is not None:
            external_file = create_decompile_external_file(
                self,
                "lsp-bridge-deno",
                "deno-uri-resolver",
                response)

            eval_in_emacs(self.define_jump_handler, external_file, get_lsp_file_host(), self.start_pos)
