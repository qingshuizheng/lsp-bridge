#!/bin/sh
# Byte-compile the elisp sources (and optionally report checkdoc problems).
#
#   scripts/check-elisp.sh                  # every tracked .el file
#   scripts/check-elisp.sh acm/acm.el ...   # just these
#   CHECKDOC=1 scripts/check-elisp.sh       # also print checkdoc findings
#
# .elc output goes to $ELC_DIR (default /tmp/lsp-bridge-elc) so the working
# tree stays clean.  Needs markdown-mode, yasnippet, tempel and compat on the
# load path -- the Nix build of Emacs in this repo already ships them.
set -eu
cd "$(dirname "$0")/.."

EMACS=${EMACS:-emacs}
ELC_DIR=${ELC_DIR:-/tmp/lsp-bridge-elc}
if [ "$#" -gt 0 ]; then
    FILES=$*
else
    # Tracked files plus the ones that are new but not committed yet, so that a
    # file added by a refactor step is compiled by the same run that reviews it;
    # --exclude-standard keeps ignored files (byte code, .venv) out.
    FILES=$(git ls-files --cached --others --exclude-standard '*.el')
fi
COUNT=$(echo "$FILES" | wc -w | tr -d ' ')

# elisp list of the file names, for --eval snippets
FILES_LISP=""
for f in $FILES; do FILES_LISP="$FILES_LISP \"$f\""; done

redir="(progn
  (make-directory \"$ELC_DIR\" t)
  (setq byte-compile-dest-file-function
        (lambda (f) (expand-file-name (concat (file-name-nondirectory f) \"c\")
                                      \"$ELC_DIR\"))))"

# A file whose last form is missing a closing paren still reads the forms
# before it, so its warning count silently drops instead of failing.  Walk the
# top level of every file first and stop on the first unbalanced one.
echo "== read every top-level form"
# shellcheck disable=SC2086
$EMACS -Q --batch -L . -L acm -l scripts/check-forms.el \
    --eval "(z/check-forms $FILES_LISP)"

echo "== byte-compile $COUNT file(s) -> $ELC_DIR"
# shellcheck disable=SC2086
$EMACS -Q --batch -L . -L acm --eval "$redir" -f batch-byte-compile $FILES

if [ "${CHECKDOC:-0}" = 1 ]; then
    echo "== checkdoc"
    $EMACS -Q --batch -L . -L acm --eval "$redir" --eval \
        "(dolist (f (list $FILES_LISP)) (checkdoc-file f))"
fi
