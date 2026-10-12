#!/bin/sh
# Run the test suites that do not need a language server.
#
#   scripts/check-test.sh          # ERT suites + the pure Python tests
#   scripts/check-test.sh --all    # also the language-server driven tests
#
# The ERT suites are read straight from the sources (`load-prefer-newer'), so a
# stale .elc left over from debugging cannot make them pass or fail wrongly.
# The Python tests run through `uv run --frozen' when `uv' is available, which
# is what provides epc/orjson/sexpdata, and through python3 otherwise.
set -eu
cd "$(dirname "$0")/.."

EMACS=${EMACS:-emacs}

run_ert() {
    "$EMACS" -Q --batch --eval "(setq load-prefer-newer t)" \
        -L . -L acm -L test -l "$1" -f ert-run-tests-batch-and-exit
}

run_python() {
    if command -v uv >/dev/null 2>&1; then
        uv run --frozen --quiet python -m unittest "$@"
    else
        python3 -m unittest "$@"
    fi
}

echo "== protocol: the two halves declare the same version (scripts/check-protocol.py)"
python3 scripts/check-protocol.py

echo "== ERT: shared core helpers (test/lsp-bridge-core-test.el)"
run_ert test/lsp-bridge-core-test.el

echo "== ERT: protocol handshake (test/lsp-bridge-protocol-test.el)"
run_ert test/lsp-bridge-protocol-test.el

echo "== ERT: acm menu (test/acm-test.el)"
run_ert test/acm-test.el

echo "== ERT: per-frame popup sessions (test/acm-session-test.el)"
run_ert test/acm-session-test.el

echo "== ERT: backend registry (test/acm-backend-test.el)"
run_ert test/lsp-bridge-completion-test.el
run_ert test/lsp-bridge-live-rename-test.el
run_ert test/acm-backend-test.el

echo "== ERT: CAPF backend and exit function (test/acm-backend-capf-test.el)"
run_ert test/acm-backend-capf-test.el

echo "== ERT: telega backend logic (test/acm-backend-telega-test.el)"
run_ert test/acm-backend-devonthink-test.el
run_ert test/acm-backend-telega-test.el

echo "== ERT: Vulpea backend (test/acm-backend-vulpea-test.el)"
run_ert test/acm-backend-vulpea-test.el

echo "== ERT: Org Babel server lookup (test/lsp-bridge-org-babel-test.el)"
run_ert test/lsp-bridge-org-babel-test.el

echo "== Python: pure tests (langserver JSON, protocol)"
run_python test.test_handlers test.test_langserver test.test_protocol test.test_textpos test.test_bridge_surface

if [ "${1:-}" = "--all" ]; then
    echo "== Python: language server driven tests"
    run_python test.test_completion test.test_find_define test.test_org_babel

    echo "== Python: byte compilation through Emacs"
    run_python test.test_byte_compile
fi
