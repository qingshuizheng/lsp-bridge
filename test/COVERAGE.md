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
| `acm/acm-backend-copilot.el` | 14 | 5 |
| `acm/acm-backend-ctags.el` | 11 | 5 |
| `acm/acm-backend-devonthink.el` | 24 | 10 |
| `acm/acm-backend-elisp.el` | 18 | 1 |
| `acm/acm-backend-jupyter.el` | 5 | 3 |
| `acm/acm-backend-lsp-workspace-symbol.el` | 7 | 3 |
| `acm/acm-backend-lsp.el` | 22 | 1 |
| `acm/acm-backend-path.el` | 5 | 5 |
| `acm/acm-backend-search-file-words.el` | 11 | 5 |
| `acm/acm-backend-tabby.el` | 7 | 4 |
| `acm/acm-backend-telega.el` | 27 | 6 |
| `acm/acm-backend-tempel.el` | 6 | 4 |
| `acm/acm-backend-vulpea.el` | 10 | 4 |
| `acm/acm-backend-yas.el` | 16 | 8 |
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
| `core/handler/solidity.py` | 27 | 6 |
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
| `lsp-bridge-dart.el` | 5 | 4 |
| `lsp-bridge-diagnostic.el` | 33 | 12 |
| `lsp-bridge-document.el` | 23 | 1 |
| `lsp-bridge-epc.el` | 62 | 1 |
| `lsp-bridge-imenu.el` | 4 | 1 |
| `lsp-bridge-inlay-hint.el` | 14 | 1 |
| `lsp-bridge-jdtls.el` | 14 | 8 |
| `lsp-bridge-jump.el` | 52 | 15 |
| `lsp-bridge-lsp-installer.el` | 4 | 0 |
| `lsp-bridge-org-babel.el` | 12 | 2 |
| `lsp-bridge-peek.el` | 62 | 1 |
| `lsp-bridge-ref.el` | 91 | 7 |
| `lsp-bridge-remote.el` | 30 | 2 |
| `lsp-bridge-rust.el` | 3 | 0 |
| `lsp-bridge-semantic-tokens.el` | 50 | 7 |
| `lsp-bridge-transport.el` | 59 | 14 |
| `lsp-bridge-ui.el` | 14 | 1 |
| `lsp-bridge-workspace.el` | 30 | 1 |
| `lsp-bridge.el` | 114 | 10 |
| `lsp_bridge.py` | 24 | 21 |

## Where the surface is still thin

Modules with at least 8 names of which a quarter or fewer are mentioned by anything
under `test/`, thinnest first.  This is the list to work from; a module is not
covered because some of it is.

| `lsp-bridge-epc.el` | 1/62 | 2% |
| `lsp-bridge-peek.el` | 1/62 | 2% |
| `lsp-bridge-workspace.el` | 1/30 | 3% |
| `lsp-bridge-document.el` | 1/23 | 4% |
| `acm/acm-backend-lsp.el` | 1/22 | 5% |
| `core/lspserver.py` | 3/64 | 5% |
| `acm/acm-backend-elisp.el` | 1/18 | 6% |
| `lsp-bridge-remote.el` | 2/30 | 7% |
| `lsp-bridge-inlay-hint.el` | 1/14 | 7% |
| `lsp-bridge-ui.el` | 1/14 | 7% |
| `core/ctags.py` | 1/13 | 8% |
| `lsp-bridge-ref.el` | 7/91 | 8% |
| `lsp-bridge.el` | 10/114 | 9% |
| `core/remote_file.py` | 3/34 | 9% |
| `acm/acm-frame.el` | 2/22 | 9% |
| `lsp-bridge-core.el` | 7/70 | 10% |
| `core/fileaction.py` | 4/39 | 10% |
| `core/utils.py` | 8/65 | 12% |
| `lsp-bridge-semantic-tokens.el` | 7/50 | 14% |
| `lsp-bridge-breadcrumb.el` | 3/21 | 14% |
| `core/search_file_words.py` | 2/12 | 17% |
| `lsp-bridge-org-babel.el` | 2/12 | 17% |
| `lsp-bridge-code-action.el` | 4/20 | 20% |
| `acm/acm-backend-telega.el` | 6/27 | 22% |
| `core/handler/solidity.py` | 6/27 | 22% |
| `lsp-bridge-call-hierarchy.el` | 7/31 | 23% |
| `core/pystardict.py` | 8/35 | 23% |
| `acm/acm-terminal.el` | 9/38 | 24% |
| `lsp-bridge-transport.el` | 14/59 | 24% |
| `lsp-bridge-completion.el` | 15/60 | 25% |

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
