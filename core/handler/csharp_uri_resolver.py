
from core.handler import Handler
from core.handler.find_define_base import create_decompile_external_file
from core.utils import *


class CSharpUriResolver(Handler):
    """Resolve C# `csharp:/metadata/` URIs to real source files.

    The metadata is fetched with `csharp/metadata`, the `source` it contains is
    written to a decompiled temp file by `create_decompile_external_file`, and
    the original define handler then jumps to the recorded position.
    """

    name = "csharp_uri_resolver"
    method = "csharp/metadata"
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
        """Write the `source` in RESPONSE to a file and jump into it."""
        if response is not None:
            external_file = create_decompile_external_file(
                self,
                "lsp-bridge-csharp",
                "csharp-uri-resolver",
                response["source"])

            eval_in_emacs(self.define_jump_handler, external_file, get_lsp_file_host(), self.start_pos)
