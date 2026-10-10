#!/bin/sh
# Check the Python half of the tree.
#
#   scripts/check-python.sh          # hard gate + the work list
#   scripts/check-python.sh --strict # also fail on the documentation gate
#
# The hard gate (syntax/undefined names, bare excepts, trailing whitespace) has
# to stay clean; the documentation gate is measured until stage S2 has reflowed
# the docstrings that predate this refactor.
set -eu
cd "$(dirname "$0")/.."
STRICT=0
[ "${1:-}" = "--strict" ] && STRICT=1
PY=$(git ls-files '*.py' | grep -v '^test/' | grep -v '^core/pystardict.py$')

echo "== py_compile"
# shellcheck disable=SC2086
python3 -m py_compile $PY

echo "== docstring presence (scripts/docstring-report.py --check)"
python3 scripts/docstring-report.py --check $PY

echo "== ruff: hard gate (E9,F63,F7,F82,E722,W291,W293)"
# shellcheck disable=SC2086
ruff check --no-cache --output-format concise \
    --select E9,F63,F7,F82,E722,W291,W293 $PY

# The rules left out here are the stylistic ones the ruff config in
# pyproject.toml deliberately ignores (module/__init__ docstrings, the
# D203/D213 and D401/D403 style choices); the CLI --select overrides the
# config's select, so the ignore list has to be repeated.
DOC_IGNORE="D100,D104,D107,D203,D213,D401,D403"

echo "== ruff: documentation gate (D), work list until S2 is done"
# shellcheck disable=SC2086
ruff check --no-cache --statistics --exit-zero --select D --ignore $DOC_IGNORE $PY

if [ "$STRICT" = 1 ]; then
    echo "== ruff: documentation gate, strict"
    # shellcheck disable=SC2086
    ruff check --no-cache --select D --ignore $DOC_IGNORE $PY
fi
