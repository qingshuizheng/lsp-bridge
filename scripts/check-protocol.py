#!/usr/bin/env python3
"""Check that the two halves of lsp-bridge speak the same protocol version.

The Emacs side declares the version in `lsp-bridge-protocol-version' (in
lsp-bridge-transport.el, where the transport and the handshake live) and the
Python side in `PROTOCOL_VERSION' (in lsp_bridge.py).
`lsp-bridge--handshake' compares them whenever the process starts and refuses to
send the server anything when they differ, so a mismatch has to be caught in a
commit rather than by a user: this is the script that does it.

The version is read from both files without importing or loading them, so the
check runs anywhere Python 3 does, and it doubles as the machine readable half
of the protocol documentation:

    scripts/check-protocol.py            # exit 1 when the two versions differ
    scripts/check-protocol.py --verbose  # also print the capabilities
"""

from __future__ import annotations

import argparse
import ast
import pathlib
import re
import sys

ROOT = pathlib.Path(__file__).resolve().parent.parent
# The constant is searched for rather than hard coded to one file, so that
# moving the transport to another file cannot break this gate silently.
ELISP_FILES = sorted(ROOT.glob("lsp-bridge*.el"))
PYTHON_FILE = ROOT / "lsp_bridge.py"

ELISP_RE = re.compile(r"^\(defconst lsp-bridge-protocol-version (\d+)", re.M)


def elisp_protocol_version() -> int:
    """Return the protocol version the Emacs side declares, and where."""
    for path in ELISP_FILES:
        match = ELISP_RE.search(path.read_text(encoding="utf-8"))
        if match:
            return int(match.group(1))
    print("no lsp-bridge-protocol-version in "
          + ", ".join(path.name for path in ELISP_FILES), file=sys.stderr)
    raise SystemExit(1)


def python_protocol_version() -> int:
    """Return the protocol version lsp_bridge.py declares."""
    tree = ast.parse(PYTHON_FILE.read_text(encoding="utf-8"), filename=str(PYTHON_FILE))
    for node in tree.body:
        if isinstance(node, ast.Assign):
            for target in node.targets:
                if isinstance(target, ast.Name) and target.id == "PROTOCOL_VERSION":
                    if isinstance(node.value, ast.Constant) and isinstance(node.value.value, int):
                        return node.value.value
    print(f"no PROTOCOL_VERSION in {PYTHON_FILE.name}", file=sys.stderr)
    raise SystemExit(1)


def python_capabilities() -> list[str]:
    """Return the capability names lsp_bridge.py declares."""
    tree = ast.parse(PYTHON_FILE.read_text(encoding="utf-8"), filename=str(PYTHON_FILE))
    for node in tree.body:
        if isinstance(node, ast.Assign):
            for target in node.targets:
                if isinstance(target, ast.Name) and target.id == "CAPABILITIES":
                    return [element.value for element in node.value.elts]
    return []


def main() -> int:
    """Compare the protocol versions of the two halves."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--verbose", action="store_true", help="print the capabilities too")
    args = parser.parse_args()

    elisp = elisp_protocol_version()
    python = python_protocol_version()
    if args.verbose:
        print(f"capabilities ({len(python_capabilities())}): "
              + ", ".join(python_capabilities()))
    if elisp != python:
        print(f"protocol version differs: lsp-bridge.el says {elisp}, "
              f"lsp_bridge.py says {python}", file=sys.stderr)
        return 1
    print(f"protocol version {elisp} in both halves")
    return 0


if __name__ == "__main__":
    sys.exit(main())
