"""Check that the names the comments and docstrings mention still exist.

A name written in a docstring as a call - ``foo()`` - or in a comment as a
reference - ``lsp-bridge-some-function`` - is a claim about the code, and a claim
that goes stale silently when the function is renamed or removed.  This collects
both and reports the ones nothing in the package defines.

Only the names this package owns are looked at: `Location' or `completionItem' is
the protocol talking, and ``json.load()`` is somebody else's function.

Run it from the repository root:

    python3 scripts/check-doc-references.py
"""

import ast
import pathlib
import re
import sys

CALL = re.compile(r"`([A-Za-z_][A-Za-z0-9_.]*)\(\)`?")
ELISP_REFERENCE = re.compile(r"(?<![\w-])((?:lsp-bridge|acm)-[a-zA-Z0-9-]+)")
ELISP_DEFINITION = re.compile(
    r"^\((?:defun|defmacro|defsubst|defvar|defvar-local|defcustom|defface|"
    r"define-minor-mode|define-derived-mode|define-globalized-minor-mode|"
    r"cl-defun|cl-defmacro|cl-defstruct|cl-defgeneric|cl-defmethod)\s+"
    r"([a-zA-Z0-9*+!?<>=/:_-]+)",
    re.M)
ELISP_FEATURE = re.compile(r"^\(provide '([a-zA-Z0-9-]+)\)", re.M)


def repository():
    """Return the root of the checkout this script lives in."""
    return pathlib.Path(__file__).resolve().parent.parent


def elisp_files(root):
    """Return the Elisp files of the package."""
    return sorted(list(root.glob("lsp-bridge*.el")) + list((root / "acm").glob("*.el")))


def python_files(root):
    """Return the Python files of the bridge, tests excluded."""
    return sorted(list((root / "core").rglob("*.py")) + [root / "lsp_bridge.py"])


def elisp_names(root):
    """Return every name the Elisp side defines, features included.

    The files are read rather than loaded, so this stays a Python check with no
    Emacs behind it; a name made by a macro of a macro would be missed, which is
    what `doc references to missing symbols' in check-doc-references.el is for.
    """
    names = set()
    for path in elisp_files(root):
        text = path.read_text(errors="ignore")
        names.update(ELISP_DEFINITION.findall(text))
        names.update(ELISP_FEATURE.findall(text))
        names.add(path.stem)
    for path in (root / "test").glob("*.el"):
        names.add(path.stem)
    return names


def python_names(paths):
    """Return every name the Python files define or bind."""
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


def documented_calls(paths):
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


def comment_of(line, marker):
    """Return the comment in LINE, or an empty string when there is none.

    Strings are dropped first, which is enough to keep a marker inside one from
    being read as the start of a comment.
    """
    stripped = re.sub(r'"[^"]*"|\'[^\']*\'', "", line)
    position = stripped.find(marker)
    return stripped[position:] if position >= 0 else ""


def comment_references(root, known):
    """Yield the references in comments to names of this package that are gone.

    A name that is only the beginning of a real one (`;; lsp-bridge-epc-server'
    over the `lsp-bridge-epc-server-...' functions) is a section header, and a
    name that ends in a dash is a placeholder such as `acm-backend-<name>'; both
    are how the sources are written down rather than a claim about code.
    """
    for path in elisp_files(root) + python_files(root):
        marker = ";" if path.suffix == ".el" else "#"
        text = path.read_text(errors="ignore")
        for number, line in enumerate(text.splitlines(), 1):
            comment = comment_of(line, marker)
            if not comment:
                continue
            for name in ELISP_REFERENCE.findall(comment):
                if name.endswith("-") or name in known:
                    continue
                if any(known_name.startswith(name) for known_name in known):
                    continue
                yield f"{path.relative_to(root)}:{number} -> `{name}'"


def main():
    """Report the references nothing defines, and fail when there are any."""
    root = repository()
    elisp = elisp_files(root)
    python = python_files(root)
    known = elisp_names(root) | python_names(python)

    calls = [(path, owner, name)
             for path, owner, name in documented_calls(python)
             if name not in known]
    comments = list(comment_references(root, known))

    print(f"docstring calls with nothing behind them: {len(calls)}")
    for path, owner, name in calls:
        print(f"  {path.name}:{owner} -> `{name}()`")
    print(f"comment references to names that are gone: {len(comments)}")
    for line in comments:
        print(f"  {line}")
    return 1 if calls or comments else 0


if __name__ == "__main__":
    sys.exit(main())
