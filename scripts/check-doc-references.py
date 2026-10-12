"""Check that the names in docstrings of the Python side still exist.

A docstring that writes a name as a call - ``foo()`` - is a claim about the code,
and a claim that goes stale silently when the function is renamed or removed.
This collects those names from every docstring under core/ and lsp_bridge.py and
reports the ones nothing in the package defines.  The LSP specification types
(`Location`, `MarkedString`) and JSON field names are data, not calls, and are
not looked at.

Run it from the repository root:

    python3 scripts/check-doc-references.py
"""

import ast
import pathlib
import re
import sys

CALL = re.compile(r"`([A-Za-z_][A-Za-z0-9_.]*)\(\)`?")


def source_files():
    """Return the Python files of the bridge, tests excluded."""
    root = pathlib.Path(__file__).resolve().parent.parent
    return sorted(list((root / "core").rglob("*.py")) + [root / "lsp_bridge.py"])


def defined_names(paths):
    """Return every name PATHs define or bind."""
    names = set()
    for path in paths:
        try:
            tree = ast.parse(path.read_text(errors="ignore"))
        except SyntaxError:
            continue
        for node in ast.walk(tree):
            if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
                names.add(node.name)
            elif isinstance(node, (ast.Name, ast.Attribute)):
                names.add(getattr(node, "id", None) or node.attr)
            elif isinstance(node, ast.arg):
                names.add(node.arg)
            elif isinstance(node, ast.alias):
                names.add((node.asname or node.name).split(".")[0])
    return {name for name in names if name}


def documented_names(paths):
    """Yield (path, owner, name) for every call a docstring names."""
    for path in paths:
        try:
            tree = ast.parse(path.read_text(errors="ignore"))
        except SyntaxError:
            continue
        for node in ast.walk(tree):
            if not isinstance(node, (ast.Module, ast.ClassDef, ast.FunctionDef)):
                continue
            docstring = ast.get_docstring(node)
            if not docstring:
                continue
            for match in CALL.findall(docstring):
                yield path, getattr(node, "name", "<module>"), match.split(".")[0]


def main():
    """Report the docstring calls that nothing defines, and fail if there are any."""
    paths = source_files()
    known = defined_names(paths)
    missing = [(path, owner, name)
               for path, owner, name in documented_names(paths)
               if name not in known]
    print(f"docstring calls checked: {len(paths)} files, missing names: {len(missing)}")
    for path, owner, name in missing:
        print(f"  {path.name}:{owner} -> `{name}()`")
    return 1 if missing else 0


if __name__ == "__main__":
    sys.exit(main())
