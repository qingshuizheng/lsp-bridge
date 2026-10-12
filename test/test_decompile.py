"""Tests for materialising a decompiled source into an external file.

A server that decompiles - the C# and Deno resolvers, among others - hands back
source code that has to exist as a file before the editor can jump to it, and this
writes it under a directory of the project's hash and tells the file action about
it so that the external file shares the server of the original one.  Where the file
lands and what the action is told is text and paths, so it is asked here with a
resolver stood in for.
"""

import hashlib
import os
import tempfile
import unittest
from core.handler.decompile import create_decompile_external_file


class FakeAction:
    """A file action that records what it was asked to do."""

    def __init__(self, project_path):
        self.project_path = project_path
        self.external = []

    def get_lsp_server_project_path(self):
        """Return the project path the server was started for."""
        return self.project_path.encode()

    def create_external_file_action(self, filepath, link):
        """Record the external file and the link it came from."""
        self.external.append((filepath, link))


class FakeResolver:
    """A uri resolver with a file action and an external link."""

    def __init__(self, project_path, link):
        self.file_action = FakeAction(project_path)
        self.external_file_link = link


class DecompileExternalFileTest(unittest.TestCase):
    """Where a decompiled source is put, and what is told about it."""

    def setUp(self):
        self.project = "/tmp/project"
        self.resolver = FakeResolver(self.project, "file:///decompiled/A.cs")

    def expected_path(self, language_dir, decompile_dir):
        """Return the path the module should choose, worked out the same way."""
        digest = hashlib.md5(self.project.encode()).hexdigest()
        return os.path.join(tempfile.gettempdir(), language_dir, digest,
                            decompile_dir, "decompiled/A.cs")

    def tearDown(self):
        # The module makes the directories under the temporary directory itself.
        import shutil
        shutil.rmtree(os.path.join(tempfile.gettempdir(), "csharp"), ignore_errors=True)

    def test_the_file_is_written_where_the_project_hash_says(self):
        path = create_decompile_external_file(self.resolver, "csharp", "decompile", "the source")
        self.assertEqual(path, self.expected_path("csharp", "decompile"))
        self.assertTrue(os.path.isfile(path))
        with open(path) as handle:
            self.assertEqual(handle.read(), "the source")

    def test_an_existing_file_is_not_written_again(self):
        first = create_decompile_external_file(self.resolver, "csharp", "decompile", "first")
        second = create_decompile_external_file(self.resolver, "csharp", "decompile", "second")
        self.assertEqual(first, second)
        with open(first) as handle:
            self.assertEqual(handle.read(), "first")

    def test_the_action_is_told_about_the_external_file(self):
        path = create_decompile_external_file(self.resolver, "csharp", "decompile", "the source")
        self.assertEqual(self.resolver.file_action.external,
                         [(path, "file:///decompiled/A.cs")])


if __name__ == "__main__":
    unittest.main()
