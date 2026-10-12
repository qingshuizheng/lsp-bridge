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

# Modules with at least this many names, and a quarter or fewer of them mentioned
# by anything under test/, are listed separately.  Counting the modules with no
# mention at all was the wrong measure: it called the package covered while some
# of its largest modules had one name in sixty touched by a test.
THIN_NAMES = 8

THIN_HEADER = f"""
## Where the surface is still thin

Modules with at least {THIN_NAMES} names of which a quarter or fewer are mentioned by anything
under `test/`, thinnest first.  This is the list to work from; a module is not
covered because some of it is.
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
  `acm-backend-tempel` tempel.  What a backend does with a list of candidates it
  already has - the option, the minimum length, the cache, the reset, the trimming
  of a label - is tested for the ones that have such a part, in
  `test/acm-backend-candidates-test.el` (the ctags, search-file-words, workspace
  symbol and Copilot backends) and in `test/acm-backend-vulpea-test.el`.
- `lsp-bridge-jdtls` asks a JDTLS server over `lsp-bridge-call-file-api`, and
  `lsp-bridge-dart` needs the Dart SDK to find its analysis server.
- `lsp-bridge-lsp-installer` downloads language servers from GitHub releases with
  `url-copy-file`: what it does is an installation, not a value a test can ask for.
- `acm-frame.el` and what draws popups need a real frame;
  `test/acm-session-test.el` covers the session bookkeeping around them, and
  `test/acm-test.el` the menu state machine that stops before a frame is touched.

### Module by module

What a module covers is in its test file, and each of those says in its commentary
what it does not, and why.  The reasons are of five kinds:

- **A peer.** The sockets, the process filter and the messages that go over them in
  `lsp-bridge-epc.el` need something on the other end - `test/lsp-bridge-epc-test.el`
  has the deferred queue, which does not - and so does the opening of a connection
  in `lsp-bridge-remote.el`, as do the requests and the formatting of
  `lsp-bridge-workspace.el`, which need the server to answer first.
- **A server process.** Starting a language server in `core/lspserver.py`, the reads
  and writes of its pipes and the dispatch of its answers need the server itself;
  `test/test_lspserver.py` has the protocol messages, the URIs and the retry
  bookkeeping around them.
- **A display.** The popup documentation of `lsp-bridge-document.el` needs one from
  the frame it makes to the `window-pixel-edges' it is positioned by, and the
  signature tooltip and the scrolling of either are the same; what is asked instead
  is what those commands do when there is nothing to show.
- **The server answering.** The search commands `lsp-bridge-ref.el` generates per
  language mode, the popup they fill, and the edit that goes back to the file being
  searched - the last of which wants that file under version control.
- **The system.** `lsp-bridge-jdtls` asks a JDTLS server, `lsp-bridge-dart` needs
  the Dart SDK, and `lsp-bridge-lsp-installer` downloads what it installs.

The live rename of `lsp-bridge-jump.el` is between the first and the fourth: the
ranges it works on are tested, and the mode itself starts from a document highlight
that the server sends.
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
    thin = []
    for path in modules(root):
        names = names_of(path)
        mentioned = sum(1 for name in names if name in tests)
        lines.append(f"| `{path.relative_to(root)}` | {len(names)} | {mentioned} |")
        if len(names) >= THIN_NAMES and mentioned * 4 <= len(names):
            thin.append((mentioned / len(names), mentioned, len(names), path.relative_to(root)))
    lines.append(THIN_HEADER)
    if thin:
        for ratio, mentioned, total, relative in sorted(thin):
            lines.append(f"| `{relative}` | {mentioned}/{total} | {ratio:.0%} |")
    else:
        lines.append("Nothing under 25% is left.")
    lines.append(FOOTER)
    (root / "test" / "COVERAGE.md").write_text("\n".join(lines))
    print(f"test/COVERAGE.md written from {len(modules(root))} modules")
    print(f"{len(thin)} of them have {THIN_NAMES} names or more and a quarter of them "
          f"mentioned or less")


if __name__ == "__main__":
    main()
