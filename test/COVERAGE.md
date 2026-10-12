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
| `acm/acm-backend-copilot.el` | 14 | 6 |
| `acm/acm-backend-ctags.el` | 11 | 8 |
| `acm/acm-backend-devonthink.el` | 24 | 10 |
| `acm/acm-backend-elisp.el` | 18 | 9 |
| `acm/acm-backend-jupyter.el` | 5 | 3 |
| `acm/acm-backend-lsp-workspace-symbol.el` | 7 | 3 |
| `acm/acm-backend-lsp.el` | 24 | 15 |
| `acm/acm-backend-path.el` | 5 | 5 |
| `acm/acm-backend-search-file-words.el` | 11 | 5 |
| `acm/acm-backend-tabby.el` | 7 | 4 |
| `acm/acm-backend-telega.el` | 27 | 6 |
| `acm/acm-backend-tempel.el` | 6 | 4 |
| `acm/acm-backend-vulpea.el` | 10 | 4 |
| `acm/acm-backend-yas.el` | 16 | 13 |
| `acm/acm-backend.el` | 20 | 19 |
| `acm/acm-frame.el` | 22 | 4 |
| `acm/acm-icon.el` | 11 | 5 |
| `acm/acm-quick-access.el` | 10 | 8 |
| `acm/acm-session.el` | 9 | 9 |
| `acm/acm-terminal.el` | 38 | 9 |
| `acm/acm.el` | 101 | 50 |
| `core/bridge_remote.py` | 16 | 9 |
| `core/bridge_search.py` | 10 | 9 |
| `core/bridge_server.py` | 15 | 5 |
| `core/copilot.py` | 17 | 8 |
| `core/ctags.py` | 13 | 12 |
| `core/fileaction.py` | 39 | 18 |
| `core/handler/breadcrumb.py` | 3 | 3 |
| `core/handler/call_hierarchy.py` | 9 | 4 |
| `core/handler/code_action.py` | 3 | 3 |
| `core/handler/completion.py` | 10 | 6 |
| `core/handler/completion_item.py` | 3 | 3 |
| `core/handler/completion_workspace_symbol.py` | 4 | 4 |
| `core/handler/csharp_uri_resolver.py` | 4 | 4 |
| `core/handler/decompile.py` | 1 | 1 |
| `core/handler/deno_uri_resolver.py` | 4 | 4 |
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
| `core/handler/jdt_uri_resolver.py` | 4 | 4 |
| `core/handler/jdtls/jdtls_add_overridable_methods.py` | 3 | 3 |
| `core/handler/jdtls/jdtls_list_overridable_methods.py` | 3 | 3 |
| `core/handler/peek.py` | 4 | 4 |
| `core/handler/prepare_rename.py` | 3 | 3 |
| `core/handler/range_formatting.py` | 3 | 2 |
| `core/handler/rename.py` | 4 | 4 |
| `core/handler/rust_expand_macro.py` | 3 | 3 |
| `core/handler/rust_reload_workspace.py` | 3 | 3 |
| `core/handler/semantic_tokens.py` | 12 | 7 |
| `core/handler/signature_help.py` | 3 | 3 |
| `core/handler/solidity.py` | 27 | 6 |
| `core/handler/textpos.py` | 9 | 7 |
| `core/handler/volar_proxy.py` | 9 | 4 |
| `core/handler/workspace_diagnostics.py` | 3 | 3 |
| `core/handler/workspace_symbol.py` | 3 | 3 |
| `core/lspserver.py` | 64 | 23 |
| `core/mergedeep.py` | 7 | 1 |
| `core/pystardict.py` | 35 | 9 |
| `core/remote_file.py` | 34 | 5 |
| `core/search_file_words.py` | 12 | 3 |
| `core/search_list.py` | 7 | 3 |
| `core/search_paths.py` | 7 | 2 |
| `core/utils.py` | 65 | 26 |
| `lsp-bridge-breadcrumb.el` | 21 | 3 |
| `lsp-bridge-call-hierarchy.el` | 31 | 7 |
| `lsp-bridge-code-action.el` | 20 | 4 |
| `lsp-bridge-completion.el` | 60 | 17 |
| `lsp-bridge-core.el` | 70 | 19 |
| `lsp-bridge-dart.el` | 5 | 4 |
| `lsp-bridge-diagnostic.el` | 33 | 12 |
| `lsp-bridge-document.el` | 23 | 10 |
| `lsp-bridge-epc.el` | 62 | 20 |
| `lsp-bridge-imenu.el` | 4 | 1 |
| `lsp-bridge-inlay-hint.el` | 15 | 10 |
| `lsp-bridge-jdtls.el` | 14 | 8 |
| `lsp-bridge-jump.el` | 55 | 28 |
| `lsp-bridge-lsp-installer.el` | 4 | 0 |
| `lsp-bridge-org-babel.el` | 12 | 2 |
| `lsp-bridge-peek.el` | 62 | 20 |
| `lsp-bridge-ref.el` | 91 | 15 |
| `lsp-bridge-remote.el` | 30 | 17 |
| `lsp-bridge-rust.el` | 3 | 3 |
| `lsp-bridge-semantic-tokens.el` | 50 | 8 |
| `lsp-bridge-transport.el` | 59 | 17 |
| `lsp-bridge-ui.el` | 14 | 12 |
| `lsp-bridge-workspace.el` | 30 | 12 |
| `lsp-bridge.el` | 114 | 17 |
| `lsp_bridge.py` | 24 | 22 |

## Where the surface is still thin

Modules with at least 8 names of which a quarter or fewer are mentioned by anything
under `test/`, thinnest first.  This is the list to work from; a module is not
covered because some of it is.

| `lsp-bridge-breadcrumb.el` | 3/21 | 14% |
| `core/remote_file.py` | 5/34 | 15% |
| `lsp-bridge.el` | 17/114 | 15% |
| `lsp-bridge-semantic-tokens.el` | 8/50 | 16% |
| `lsp-bridge-ref.el` | 15/91 | 16% |
| `lsp-bridge-org-babel.el` | 2/12 | 17% |
| `acm/acm-frame.el` | 4/22 | 18% |
| `lsp-bridge-code-action.el` | 4/20 | 20% |
| `acm/acm-backend-telega.el` | 6/27 | 22% |
| `core/handler/solidity.py` | 6/27 | 22% |
| `lsp-bridge-call-hierarchy.el` | 7/31 | 23% |
| `acm/acm-terminal.el` | 9/38 | 24% |
| `core/search_file_words.py` | 3/12 | 25% |

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

### Module by module

What a module covers is in its test file, and each of those says in its commentary
what it does not, and why.  The reasons are of five kinds:

- **A peer.** The sockets, the process filter and the messages that go over them in
  `lsp-bridge-epc.el` need something on the other end - `test/lsp-bridge-epc-test.el`
  has the deferred queue, which does not - and so does the opening of a connection
  in `lsp-bridge-remote.el`, as do the requests and the formatting of
  `lsp-bridge-workspace.el`, which need the server to answer first.
- **A server process.** Starting a language server in `core/lspserver.py`, the reads
  and writes of its pipes and the dispatch of its answers need the server itself;
  `test/test_lspserver.py` has the protocol messages, the URIs and the retry
  bookkeeping around them.
- **A display.** The popup documentation of `lsp-bridge-document.el` needs one from
  the frame it makes to the `window-pixel-edges' it is positioned by, and the
  signature tooltip and the scrolling of either are the same; what is asked instead
  is what those commands do when there is nothing to show.
- **The server answering.** The search commands `lsp-bridge-ref.el` generates per
  language mode, the popup they fill, and the edit that goes back to the file being
  searched - the last of which wants that file under version control.
- **The system.** `lsp-bridge-jdtls` asks a JDTLS server, `lsp-bridge-dart` needs
  the Dart SDK, and `lsp-bridge-lsp-installer` downloads what it installs.

The live rename of `lsp-bridge-jump.el` is between the first and the fourth: the
ranges it works on are tested, and the mode itself starts from a document highlight
that the server sends.
