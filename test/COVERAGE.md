# Local test coverage

What can be checked without a language server, per module: how many of a
module's names (`defun`/`defvar`/`defcustom` for Elisp, classes, methods and
functions for Python) are mentioned by anything under `test/`, and how many
are.

This is a map for finding gaps, not a score: a name that a test never mentions
may still be exercised through another entry point, and a module with a thin
ratio may be mostly wiring that only a server can drive.

Regenerate the table with:

```sh
python3 - <<'PY'
… see the script in the commit that added this file …
PY
```

| module | names | mentioned |
| --- | ---: | ---: |
| `acm/acm-backend-capf.el` | 6 | 3 |
| `acm/acm-backend-copilot.el` | 13 | 0 |
| `acm/acm-backend-ctags.el` | 10 | 0 |
| `acm/acm-backend-devonthink.el` | 23 | 10 |
| `acm/acm-backend-elisp.el` | 17 | 0 |
| `acm/acm-backend-jupyter.el` | 4 | 0 |
| `acm/acm-backend-lsp-workspace-symbol.el` | 7 | 0 |
| `acm/acm-backend-lsp.el` | 19 | 0 |
| `acm/acm-backend-path.el` | 4 | 0 |
| `acm/acm-backend-search-file-words.el` | 10 | 0 |
| `acm/acm-backend-tabby.el` | 6 | 0 |
| `acm/acm-backend-telega.el` | 20 | 5 |
| `acm/acm-backend-tempel.el` | 5 | 0 |
| `acm/acm-backend-vulpea.el` | 10 | 4 |
| `acm/acm-backend-yas.el` | 16 | 0 |
| `acm/acm-backend.el` | 20 | 19 |
| `acm/acm-frame.el` | 22 | 1 |
| `acm/acm-icon.el` | 11 | 3 |
| `acm/acm-quick-access.el` | 10 | 3 |
| `acm/acm-session.el` | 9 | 9 |
| `acm/acm.el` | 87 | 29 |
| `core/bridge_remote.py` | 14 | 9 |
| `core/bridge_search.py` | 10 | 9 |
| `core/bridge_server.py` | 15 | 5 |
| `core/copilot.py` | 16 | 6 |
| `core/ctags.py` | 12 | 1 |
| `core/fileaction.py` | 37 | 4 |
| `core/handler/breadcrumb.py` | 3 | 0 |
| `core/handler/call_hierarchy.py` | 9 | 0 |
| `core/handler/code_action.py` | 3 | 0 |
| `core/handler/completion.py` | 10 | 1 |
| `core/handler/completion_item.py` | 3 | 0 |
| `core/handler/completion_workspace_symbol.py` | 4 | 0 |
| `core/handler/csharp_uri_resolver.py` | 3 | 0 |
| `core/handler/decompile.py` | 1 | 0 |
| `core/handler/deno_uri_resolver.py` | 3 | 0 |
| `core/handler/diagnostic.py` | 3 | 0 |
| `core/handler/document_highlight.py` | 3 | 0 |
| `core/handler/document_symbol.py` | 3 | 0 |
| `core/handler/execute_command.py` | 4 | 0 |
| `core/handler/find_define.py` | 3 | 0 |
| `core/handler/find_implementation.py` | 3 | 0 |
| `core/handler/find_references.py` | 3 | 0 |
| `core/handler/find_type_define.py` | 3 | 0 |
| `core/handler/formatting.py` | 3 | 0 |
| `core/handler/hover.py` | 5 | 0 |
| `core/handler/imenu.py` | 3 | 0 |
| `core/handler/inlay_hint.py` | 3 | 0 |
| `core/handler/jdt_uri_resolver.py` | 3 | 0 |
| `core/handler/jdtls/jdtls_add_overridable_methods.py` | 3 | 0 |
| `core/handler/jdtls/jdtls_list_overridable_methods.py` | 3 | 0 |
| `core/handler/peek.py` | 4 | 0 |
| `core/handler/prepare_rename.py` | 3 | 0 |
| `core/handler/range_formatting.py` | 3 | 0 |
| `core/handler/rename.py` | 4 | 1 |
| `core/handler/rust_expand_macro.py` | 3 | 0 |
| `core/handler/rust_reload_workspace.py` | 3 | 0 |
| `core/handler/semantic_tokens.py` | 11 | 0 |
| `core/handler/signature_help.py` | 3 | 0 |
| `core/handler/solidity.py` | 1 | 0 |
| `core/handler/textpos.py` | 9 | 7 |
| `core/handler/volar_proxy.py` | 6 | 0 |
| `core/handler/workspace_diagnostics.py` | 3 | 0 |
| `core/handler/workspace_symbol.py` | 3 | 0 |
| `core/lspserver.py` | 61 | 3 |
| `core/mergedeep.py` | 2 | 1 |
| `core/pystardict.py` | 16 | 8 |
| `core/remote_file.py` | 33 | 3 |
| `core/search_file_words.py` | 11 | 2 |
| `core/search_list.py` | 6 | 2 |
| `core/search_paths.py` | 6 | 1 |
| `core/utils.py` | 60 | 8 |
| `lsp-bridge-breadcrumb.el` | 15 | 0 |
| `lsp-bridge-call-hierarchy.el` | 29 | 0 |
| `lsp-bridge-code-action.el` | 20 | 0 |
| `lsp-bridge-completion.el` | 56 | 11 |
| `lsp-bridge-core.el` | 59 | 5 |
| `lsp-bridge-dart.el` | 4 | 0 |
| `lsp-bridge-diagnostic.el` | 30 | 0 |
| `lsp-bridge-document.el` | 23 | 1 |
| `lsp-bridge-epc.el` | 62 | 1 |
| `lsp-bridge-imenu.el` | 4 | 0 |
| `lsp-bridge-inlay-hint.el` | 11 | 0 |
| `lsp-bridge-jdtls.el` | 14 | 0 |
| `lsp-bridge-jump.el` | 45 | 11 |
| `lsp-bridge-lsp-installer.el` | 4 | 0 |
| `lsp-bridge-org-babel.el` | 8 | 2 |
| `lsp-bridge-peek.el` | 62 | 0 |
| `lsp-bridge-ref.el` | 91 | 0 |
| `lsp-bridge-remote.el` | 24 | 2 |
| `lsp-bridge-rust.el` | 3 | 0 |
| `lsp-bridge-semantic-tokens.el` | 44 | 0 |
| `lsp-bridge-transport.el` | 57 | 14 |
| `lsp-bridge-ui.el` | 13 | 1 |
| `lsp-bridge-workspace.el` | 30 | 1 |
| `lsp-bridge.el` | 113 | 10 |
| `lsp_bridge.py` | 22 | 21 |

## What cannot be covered locally

- The LSP driven commands (`test_completion`, `test_find_define`, `test_org_babel`)
  need a real language server and are run by `scripts/check-test.sh --all`.
- The backends that talk to another application (`acm-backend-telega`,
  `acm-backend-devonthink`, `acm-backend-vulpea`, `acm-backend-yas`,
  `acm-backend-tempel`, `acm-backend-copilot`, `acm-backend-tabby`,
  `acm-backend-jupyter`) are tested where their own logic allows it, and the
  rest needs the service they belong to.
- `acm-frame.el` and what draws popups need a real frame; `test/acm-session-test.el`
  covers the session bookkeeping around them.
