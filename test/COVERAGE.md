# Local test coverage

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

| `acm/acm-backend-capf.el` | 6 | 3 |
| `acm/acm-backend-copilot.el` | 14 | 0 |
| `acm/acm-backend-ctags.el` | 11 | 5 |
| `acm/acm-backend-devonthink.el` | 24 | 10 |
| `acm/acm-backend-elisp.el` | 18 | 1 |
| `acm/acm-backend-jupyter.el` | 5 | 0 |
| `acm/acm-backend-lsp-workspace-symbol.el` | 7 | 0 |
| `acm/acm-backend-lsp.el` | 22 | 1 |
| `acm/acm-backend-path.el` | 5 | 5 |
| `acm/acm-backend-search-file-words.el` | 11 | 5 |
| `acm/acm-backend-tabby.el` | 7 | 0 |
| `acm/acm-backend-telega.el` | 27 | 6 |
| `acm/acm-backend-tempel.el` | 5 | 0 |
| `acm/acm-backend-vulpea.el` | 10 | 4 |
| `acm/acm-backend-yas.el` | 16 | 0 |
| `acm/acm-backend.el` | 20 | 19 |
| `acm/acm-frame.el` | 22 | 2 |
| `acm/acm-icon.el` | 11 | 5 |
| `acm/acm-quick-access.el` | 10 | 8 |
| `acm/acm-session.el` | 9 | 9 |
| `acm/acm-terminal.el` | 38 | 9 |
| `acm/acm.el` | 101 | 49 |
| `core/bridge_remote.py` | 16 | 9 |
| `core/bridge_search.py` | 10 | 9 |
| `core/bridge_server.py` | 15 | 5 |
| `core/copilot.py` | 17 | 6 |
| `core/ctags.py` | 13 | 1 |
| `core/fileaction.py` | 39 | 4 |
| `core/handler/breadcrumb.py` | 3 | 3 |
| `core/handler/call_hierarchy.py` | 9 | 4 |
| `core/handler/code_action.py` | 3 | 3 |
| `core/handler/completion.py` | 10 | 6 |
| `core/handler/completion_item.py` | 3 | 3 |
| `core/handler/completion_workspace_symbol.py` | 4 | 4 |
| `core/handler/csharp_uri_resolver.py` | 4 | 3 |
| `core/handler/decompile.py` | 1 | 0 |
| `core/handler/deno_uri_resolver.py` | 4 | 3 |
| `core/handler/diagnostic.py` | 3 | 3 |
| `core/handler/document_highlight.py` | 3 | 3 |
| `core/handler/document_symbol.py` | 3 | 3 |
| `core/handler/execute_command.py` | 4 | 4 |
| `core/handler/find_define.py` | 3 | 3 |
| `core/handler/find_implementation.py` | 3 | 3 |
| `core/handler/find_references.py` | 3 | 3 |
| `core/handler/find_type_define.py` | 3 | 3 |
| `core/handler/formatting.py` | 3 | 2 |
| `core/handler/hover.py` | 5 | 5 |
| `core/handler/imenu.py` | 3 | 3 |
| `core/handler/inlay_hint.py` | 3 | 3 |
| `core/handler/jdt_uri_resolver.py` | 4 | 3 |
| `core/handler/jdtls/jdtls_add_overridable_methods.py` | 3 | 3 |
| `core/handler/jdtls/jdtls_list_overridable_methods.py` | 3 | 3 |
| `core/handler/peek.py` | 4 | 4 |
| `core/handler/prepare_rename.py` | 3 | 3 |
| `core/handler/range_formatting.py` | 3 | 2 |
| `core/handler/rename.py` | 4 | 4 |
| `core/handler/rust_expand_macro.py` | 3 | 3 |
| `core/handler/rust_reload_workspace.py` | 3 | 3 |
| `core/handler/semantic_tokens.py` | 12 | 6 |
| `core/handler/signature_help.py` | 3 | 3 |
| `core/handler/solidity.py` | 27 | 0 |
| `core/handler/textpos.py` | 9 | 7 |
| `core/handler/volar_proxy.py` | 9 | 3 |
| `core/handler/workspace_diagnostics.py` | 3 | 3 |
| `core/handler/workspace_symbol.py` | 3 | 3 |
| `core/lspserver.py` | 64 | 3 |
| `core/mergedeep.py` | 7 | 1 |
| `core/pystardict.py` | 35 | 8 |
| `core/remote_file.py` | 34 | 3 |
| `core/search_file_words.py` | 12 | 2 |
| `core/search_list.py` | 7 | 2 |
| `core/search_paths.py` | 7 | 1 |
| `core/utils.py` | 65 | 8 |
| `lsp-bridge-breadcrumb.el` | 21 | 3 |
| `lsp-bridge-call-hierarchy.el` | 31 | 7 |
| `lsp-bridge-code-action.el` | 20 | 4 |
| `lsp-bridge-completion.el` | 60 | 15 |
| `lsp-bridge-core.el` | 70 | 7 |
| `lsp-bridge-dart.el` | 5 | 0 |
| `lsp-bridge-diagnostic.el` | 33 | 12 |
| `lsp-bridge-document.el` | 23 | 1 |
| `lsp-bridge-epc.el` | 62 | 1 |
| `lsp-bridge-imenu.el` | 4 | 1 |
| `lsp-bridge-inlay-hint.el` | 14 | 1 |
| `lsp-bridge-jdtls.el` | 14 | 0 |
| `lsp-bridge-jump.el` | 52 | 15 |
| `lsp-bridge-lsp-installer.el` | 4 | 0 |
| `lsp-bridge-org-babel.el` | 12 | 2 |
| `lsp-bridge-peek.el` | 62 | 1 |
| `lsp-bridge-ref.el` | 91 | 0 |
| `lsp-bridge-remote.el` | 30 | 2 |
| `lsp-bridge-rust.el` | 3 | 0 |
| `lsp-bridge-semantic-tokens.el` | 50 | 0 |
| `lsp-bridge-transport.el` | 59 | 14 |
| `lsp-bridge-ui.el` | 14 | 1 |
| `lsp-bridge-workspace.el` | 30 | 1 |
| `lsp-bridge.el` | 114 | 10 |
| `lsp_bridge.py` | 24 | 21 |

## What cannot be covered locally

- The LSP driven commands (`test_completion`, `test_find_define`,
  `test_org_babel`) need a real language server and run under
  `scripts/check-test.sh --all`.
- The backends that talk to another application (`acm-backend-telega`,
  `acm-backend-devonthink`, `acm-backend-vulpea`, `acm-backend-yas`,
  `acm-backend-tempel`, `acm-backend-copilot`, `acm-backend-tabby`,
  `acm-backend-jupyter`) are tested where their own logic allows it, and the rest
  needs the service they belong to.
- `acm-frame.el` and what draws popups need a real frame;
  `test/acm-session-test.el` covers the session bookkeeping around them.
