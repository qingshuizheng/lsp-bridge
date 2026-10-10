"""Materialise a decompiled source so it can be jumped to.

The C# and Deno resolvers turn a compiled artefact into a temporary file
that the editor can open like any other source.
"""

import hashlib
import os
import tempfile
from urllib.parse import urlparse, unquote

from core.utils import *

__all__ = [
    "create_decompile_external_file",
]


def create_decompile_external_file(uri_resolver, language_dir, decompile_dir, source_code):
    """Write decompiled SOURCE_CODE to an external file and return its URI.

    Some LSP servers, such as Java or C#, need to decompile source code to an
    *external* file before jumping to a definition in the *original* file.
    Base on lsp-bridge default policy, lsp-bridge will create NEW LSP server for *external* file if *external* file own by different directory.
    If *external* file use *new* LSP server, `lsp-bridge-find-def` won't response in *external* file,
    because *new* LSP server don't know project information about *original* project.

    So we call `create_external_file_action` on *external* file, pass *original* file's LSP server to FileAction structural function,
    to make sure *external* file share SAME LSP server with *original* file.

    Last, we can do `lsp-bridge-find-def` in *external* file.
    """
    # Init.
    external_file_dir = ''

    # Build temp directory base on project hash.
    md5 = hashlib.md5()
    md5.update(uri_resolver.file_action.get_lsp_server_project_path())
    project_hash = md5.hexdigest()
    data_dir = pathlib.Path(os.path.join(tempfile.gettempdir(), language_dir, project_hash))
    external_file_dir = data_dir / decompile_dir

    # Build temp file path.
    url = urlparse(uri_resolver.external_file_link)
    path = unquote(url.path)[1:] # remove first / from path to make `external_file_dir` can join `path` at below
    external_file = external_file_dir / path

    # Create temp file if is not exists.
    external_file.parent.mkdir(exist_ok=True, parents=True)

    # Write source code to temp file.
    if not external_file.exists():
        with open(external_file, 'w') as f:
            f.write(source_code)

    external_file = external_file.as_posix()

    # Create external file action, make sure external file share one LSP server with current file.
    uri_resolver.file_action.create_external_file_action(external_file, uri_resolver.external_file_link)

    return external_file
