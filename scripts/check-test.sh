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

echo "== docstrings: names they mention still exist (scripts/check-doc-references.*)"
"$EMACS" -Q --batch -L . -L acm -l scripts/check-doc-references.el
python3 scripts/check-doc-references.py

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

echo "== ERT: finding textual occurrences (test/lsp-bridge-jump-test.el)"
run_ert test/lsp-bridge-jump-test.el

echo "== ERT: paths, symbols and highlights (test/lsp-bridge-workspace-test.el)"
run_ert test/lsp-bridge-workspace-test.el

echo "== ERT: naming and decoding remote files (test/lsp-bridge-remote-test.el)"
run_ert test/lsp-bridge-remote-test.el

echo "== ERT: the deferred queue behind every request (test/lsp-bridge-epc-test.el)"
run_ert test/lsp-bridge-epc-test.el

echo "== ERT: the closing labels of Dart (test/lsp-bridge-dart-test.el)"
run_ert test/lsp-bridge-dart-test.el

echo "== ERT: where JDTLS keeps its caches (test/lsp-bridge-jdtls-test.el)"
run_ert test/lsp-bridge-jdtls-test.el

echo "== ERT: yasnippet template labelling (test/acm-backend-yas-test.el)"
run_ert test/acm-backend-yas-test.el

echo "== ERT: the reference buffer (test/lsp-bridge-ref-test.el)"
run_ert test/lsp-bridge-ref-test.el

echo "== ERT: semantic tokens on the buffer (test/lsp-bridge-semantic-tokens-test.el)"
run_ert test/lsp-bridge-semantic-tokens-test.el

echo "== ERT: diagnostics over the buffer (test/lsp-bridge-diagnostic-test.el)"
run_ert test/lsp-bridge-diagnostic-test.el

echo "== ERT: what the small backends offer (test/acm-backend-candidates-test.el)"
run_ert test/acm-backend-candidates-test.el

echo "== ERT: breadcrumb icons (test/lsp-bridge-breadcrumb-test.el)"
run_ert test/lsp-bridge-breadcrumb-test.el

echo "== ERT: terminal menu front end (test/acm-terminal-test.el)"
run_ert test/acm-terminal-test.el

echo "== ERT: Elisp search backend (test/acm-backend-elisp-test.el)"
run_ert test/acm-backend-elisp-test.el

echo "== ERT: path backend (test/acm-backend-path-test.el)"
run_ert test/acm-backend-path-test.el

echo "== ERT: backend registry (test/acm-backend-test.el)"
run_ert test/lsp-bridge-call-hierarchy-test.el
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
run_python test.test_handlers test.test_solidity test.test_langserver test.test_protocol test.test_textpos test.test_bridge_surface

if [ "${1:-}" = "--all" ]; then
    echo "== Python: language server driven tests"
    run_python test.test_completion test.test_find_define test.test_org_babel

    echo "== Python: byte compilation through Emacs"
    run_python test.test_byte_compile
fi
