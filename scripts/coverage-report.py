"""Write test/COVERAGE.md, the map of what the local tests reach.

For every module of the package, the table says how many of its names (functions,
variables and faces for Elisp; classes, functions and methods for Python) are
mentioned by anything under test/.  It is a map for finding gaps, not a score: a
name the tests never mention can still be exercised through another entry point -
`process_request' is shared by every handler and is counted for each of them -
and a module with a thin ratio can be wiring only a server can drive.

Usage, from the repository root:

    python3 scripts/coverage-report.py
"""

import pathlib
import re

ELISP_NAME = re.compile(
    r"^\((?:defun|defmacro|defsubst|defvar|defvar-local|defcustom|defface)\s+"
    r"([a-zA-Z0-9*+!?<>=/:_-]+)",
    re.M)
PYTHON_NAME = re.compile(r"^(?:class|def)\s+([A-Za-z0-9_]+)", re.M)
PYTHON_METHOD = re.compile(r"^\s{4}def\s+([A-Za-z0-9_]+)", re.M)

HEADER = """# Local test coverage

What can be checked without a language server, per module: how many of a module's
names are mentioned by anything under `test/`, and how many are.

This is a map for finding gaps, not a score: a name that no test mentions may
still be exercised through another entry point (every handler shares
`process_request`, and it is counted for each of them), and a module with a thin
ratio may be mostly wiring that only a server can drive.

Regenerate it with:

```sh
python3 scripts/coverage-report.py
```

| module | names | mentioned |
| --- | ---: | ---: |
"""

FOOTER = """
## What cannot be covered locally

- The LSP driven commands (`test_completion`, `test_find_define`,
  `test_org_babel`) need a real language server and run under
  `scripts/check-test.sh --all`.
- The part of a backend that produces candidates belongs to the service it is
  for, and cannot be asked here: `acm-backend-yas` requires yasnippet,
  `acm-backend-tabby` requires tabby, `acm-backend-jupyter` requires jupyter and
  its org client, `acm-backend-copilot` a Copilot through the Python side, and
  `acm-backend-tempel` tempel.  What those backends do with the candidates once
  they have them - the option, the minimum length, the cache, the reset, the
  trimming of a label - is what `test/acm-backend-candidates-test.el` asks.
- `lsp-bridge-jdtls` asks a JDTLS server over `lsp-bridge-call-file-api`, and
  `lsp-bridge-dart` needs the Dart SDK to find its analysis server.
- `lsp-bridge-lsp-installer` downloads language servers from GitHub releases with
  `url-copy-file`: what it does is an installation, not a value a test can ask for.
- `acm-frame.el` and what draws popups need a real frame;
  `test/acm-session-test.el` covers the session bookkeeping around them, and
  `test/acm-test.el` the menu state machine that stops before a frame is touched.
"""


def modules(root):
    """Return the source modules of the package, in a stable order."""
    return sorted(list(root.glob("lsp-bridge*.el"))
                  + list((root / "acm").glob("*.el"))
                  + [path for path in (root / "core").rglob("*.py")
                     if path.name != "__init__.py"]
                  + [root / "lsp_bridge.py"])


def names_of(path):
    """Return the names PATH defines."""
    text = path.read_text(errors="ignore")
    if path.suffix == ".el":
        return sorted(set(ELISP_NAME.findall(text)))
    return sorted(set(PYTHON_NAME.findall(text) + PYTHON_METHOD.findall(text)))


def main():
    """Write the coverage map."""
    root = pathlib.Path(__file__).resolve().parent.parent
    tests = "\n".join(path.read_text(errors="ignore")
                      for path in (root / "test").glob("*") if path.is_file())

    lines = [HEADER]
    for path in modules(root):
        names = names_of(path)
        mentioned = sum(1 for name in names if name in tests)
        lines.append(f"| `{path.relative_to(root)}` | {len(names)} | {mentioned} |")
    lines.append(FOOTER)
    (root / "test" / "COVERAGE.md").write_text("\n".join(lines))
    print(f"test/COVERAGE.md written from {len(modules(root))} modules")


if __name__ == "__main__":
    main()
