import json
import os
import unittest
from pathlib import Path
from unittest.mock import patch

from core.utils import replace_template
from test.common import BASE_DIR

_LANG_SERVER_DIR = BASE_DIR / "langserver"
_MULTI_SERVER_DIR = BASE_DIR / "multiserver"


class LangServerJson(unittest.TestCase):
    def test_json_content(self) -> None:
        def _check_json(p: Path) -> None:
            with open(p, "r", encoding="utf-8") as f:
                json.load(f)

        for d in (_LANG_SERVER_DIR, _MULTI_SERVER_DIR):
            for f in d.iterdir():
                if f.is_file() and f.suffix == ".json":
                    with self.subTest(file=f):
                        _check_json(f)

    def test_powershell_editor_services_config(self) -> None:
        config_path = _LANG_SERVER_DIR / "powershell-editor-services.json"
        with open(config_path, "r", encoding="utf-8") as f:
            config = json.load(f)

        self.assertEqual(config["name"], "powershell-editor-services")
        self.assertEqual(config["languageId"], "powershell")
        self.assertIn("-Stdio", config["command"])
        self.assertIn(
            "$LSP_BRIDGE_POWERSHELL_EDITOR_SERVICES_DIR/PowerShellEditorServices/Start-EditorServices.ps1",
            config["command"],
        )
        self.assertIn("$LSP_BRIDGE_POWERSHELL_LOG_PATH", config["command"])
        self.assertIn(
            "$LSP_BRIDGE_POWERSHELL_SESSION_DETAILS_PATH", config["command"]
        )

    def test_command_template_expands_environment_variables(self) -> None:
        with patch.dict(os.environ, {"LSP_BRIDGE_TEST_DIR": "/tmp/lsp-bridge"}):
            self.assertEqual(
                replace_template("$LSP_BRIDGE_TEST_DIR/server"),
                "/tmp/lsp-bridge/server",
            )


class RepositoryLayout(unittest.TestCase):
    """The directories the server reads are found relative to the repository.

    `get_lang_server_path' resolved its directory from its own `__file__', which
    was correct while it lived in lsp_bridge.py at the root and silently wrong
    once it moved into core/: every server lookup then looked inside
    core/langserver and core/multiserver.  Nothing caught it, because the tests
    that need a language server are the only ones that look up a configuration.
    """

    def test_the_configuration_directories_are_under_the_repository_root(self):
        from core.utils import REPO_ROOT

        for name in ("langserver", "multiserver", "resources"):
            with self.subTest(directory=name):
                self.assertTrue((REPO_ROOT / name).is_dir(), f"{name}/ is missing under {REPO_ROOT}")

    def test_the_repository_root_is_not_a_package_directory(self):
        from core.utils import REPO_ROOT

        # the failure above put every lookup one level too deep
        self.assertNotEqual(REPO_ROOT.name, "core")
        self.assertEqual(REPO_ROOT, Path(BASE_DIR))

    def test_the_multiserver_configuration_the_tests_use_is_found(self):
        # this is the file the first failing build could not find, because the
        # lookup was resolved from core/ instead of the repository root
        from core.utils import REPO_ROOT

        self.assertTrue((REPO_ROOT / "multiserver" / "basedpyright_ruff.json").is_file())
