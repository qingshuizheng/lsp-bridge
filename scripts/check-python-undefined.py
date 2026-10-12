"""Check that the names the Python side uses are defined somewhere in it.

A name written in the code and defined nowhere is not a style question: reading it
raises `NameError' the moment the line is reached, and a line that is only reached for
some files can go unnoticed for a long time.  Two of them were found in the Solidity
fallback - `_SOLIDITY_BUILTIN_NAMESPACES', which is defined now, and
`_YUL_BUILTIN_FUNCTIONS', which is not - which is what this check is for.

Only names of the shape this package uses for its constants are looked at, that is a
leading underscore and capitals: a name defined in another module and brought in with
`from ... import *' cannot be told from one that is missing, so what is reported is the
narrow case that is certain.

Run it from the repository root:

    python3 scripts/check-python-undefined.py
"""

import pathlib
import re
import sys

DEFINED = re.compile(
    r"(?:^|\n)\s*(?:(?:def|class)\s+([A-Za-z_][A-Za-z0-9_]*)"
    r"|([A-Za-z_][A-Za-z0-9_]*)\s*(?::[^=\n]+)?="
    r"|import\s+([A-Za-z_][A-Za-z0-9_]*)"
    r"|for\s+([A-Za-z_][A-Za-z0-9_]*)\s+in"
    r"|as\s+([A-Za-z_][A-Za-z0-9_]*))"
)
USED = re.compile(r"(?<![\w.])(_[A-Z][A-Z0-9_]*)")

KNOWN = {
    # The built-in functions of Yul, which the module expects to be written down
    # somewhere: the list is long and belongs to the compiler rather than to this
    # package, so it is kept here until it is written down from a source that has it.
    "_YUL_BUILTIN_FUNCTIONS",
}


def repository():
    """Return the root of the checkout this script lives in."""
    return pathlib.Path(__file__).resolve().parent.parent


def python_files(root):
    """Return the Python files of the bridge, tests excluded."""
    return sorted(list((root / "core").rglob("*.py")) + [root / "lsp_bridge.py"])


def main():
    """Report the names that are used and defined nowhere, and fail when there are."""
    root = repository()
    defined, used = set(), {}
    for path in python_files(root):
        if not path.exists():
            continue
        text = path.read_text(errors="ignore")
        for groups in DEFINED.findall(text):
            defined |= {name for name in groups if name}
        for name in USED.findall(text):
            used.setdefault(name, set()).add(path.relative_to(root))

    # A name that a defined name begins with is a part of it, as `_S' is of the
    # classes named `_StarDict...'.
    missing = {name: where for name, where in used.items()
               if name not in defined
               and name not in KNOWN
               and not any(other.startswith(name) for other in defined)}

    print(f"names used and defined nowhere: {len(missing)}")
    for name, where in sorted(missing.items()):
        print(f"  {name}: {', '.join(sorted(str(w) for w in where))}")
    print(f"known and left for now: {len(KNOWN)}")
    for name in sorted(KNOWN):
        print(f"  {name}")
    return 1 if missing else 0


if __name__ == "__main__":
    sys.exit(main())
