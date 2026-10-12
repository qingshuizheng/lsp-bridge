;;; check-doc-references.el --- Check docstrings against the code  -*- lexical-binding: t; -*-

;;; Commentary:
;; Two things go stale in a docstring without anybody noticing: the name of
;; something that was renamed or removed, and the name of an argument the
;; function no longer takes.  This walks every symbol this package defines, reads
;; the docstring the loaded definition really has, and reports:
;;
;; - references to `symbols' of this package that no longer exist, which fail the
;;   run, since a docstring that names a function that is gone is wrong;
;; - uppercase words that are neither arguments of the function nor in the list
;;   of acronyms below, printed for information only: an argument is named in
;;   uppercase by convention, so a new one that is mentioned but not taken is
;;   worth looking at, while the list keeps the prose quiet.
;;
;; Run it from the repository root:
;;
;;   emacs -Q --batch -L . -L acm -l scripts/check-doc-references.el
;;
;;; Code:

(defconst lsp-bridge-doc-check--prefixes '("lsp-bridge" "acm")
  "Symbol name prefixes this package defines.")

(defconst lsp-bridge-doc-check--acronyms
  '("ANSI" "ASCII" "CL" "CPU" "CR" "DATA" "DEL" "DIR" "EL" "ELISP" "EOF" "EOL"
    "EPC" "ESC" "FN" "GC" "GLOBS" "GUI" "HTTP" "HTTPS" "ID" "IDS" "IP" "JDTLS"
    "JSON" "KEY" "KEYS" "LBR" "LF" "LHS" "LSP" "M-H" "M-N" "M-P" "NOTE" "OK"
    "OS" "PNG" "PTY" "RET" "RGB" "RHS" "RPC" "RSS" "SPC" "SQL" "SSH" "STDERR"
    "STDIN" "STDOUT" "SVG" "TAB" "TCP" "TODO" "TRAMP" "TTY" "TUI" "URI" "URIS"
    "URL" "URLS" "UTF-8" "UUID" "X" "XML" "Y" "Z")
  "Uppercase words that are documentation, not argument names.")

(defun lsp-bridge-doc-check--own-p (symbol)
  "Return non-nil when SYMBOL belongs to this package."
  (seq-some (lambda (prefix) (string-prefix-p prefix (symbol-name symbol)))
            lsp-bridge-doc-check--prefixes))

(defun lsp-bridge-doc-check--load ()
  "Load the package so the docstrings are the ones that are really there."
  (setq load-prefer-newer t)
  (let ((default-directory (or (locate-dominating-file default-directory "lsp-bridge.el")
                               default-directory)))
    (dolist (file (append (directory-files default-directory t "^lsp-bridge.*\\.el$")
                          (directory-files (expand-file-name "acm" default-directory)
                                           t "\\.el$")))
      (condition-case nil (load file nil t) (error nil)))))

(defun lsp-bridge-doc-check--missing-symbols ()
  "Return the docstrings that name a symbol of this package that is gone."
  (let ((case-fold-search nil)
        found)
    (mapatoms
     (lambda (symbol)
       (when (lsp-bridge-doc-check--own-p symbol)
         (let ((doc (ignore-errors (documentation symbol))))
           (when doc
             (let ((position 0))
               (while (string-match "`\\([a-zA-Z0-9*+!?<>=/:_-]+\\)'?" doc position)
                 (let ((reference (intern (match-string 1 doc))))
                   (setq position (match-end 0))
                   (when (and (lsp-bridge-doc-check--own-p reference)
                              (not (boundp reference))
                              (not (fboundp reference))
                              (not (facep reference)))
                     (push (format "%s -> `%s'" symbol reference) found))))))))))
    (nreverse found)))

(defun lsp-bridge-doc-check--unknown-arguments ()
  "Return the uppercase words that are not arguments and not known acronyms."
  (let ((case-fold-search nil)
        found)
    (mapatoms
     (lambda (symbol)
       (when (and (lsp-bridge-doc-check--own-p symbol)
                  (fboundp symbol)
                  (not (macrop symbol)))
         (let ((doc (ignore-errors (documentation symbol)))
               (arguments (ignore-errors (help-function-arglist symbol t))))
           (when (and doc (listp arguments))
             (let ((position 0)
                   (lowered (mapcar (lambda (argument)
                                      (if (symbolp argument)
                                          (intern (downcase (symbol-name argument)))
                                        argument))
                                    arguments)))
               (while (string-match "\\b\\([A-Z][A-Z0-9-]+\\)\\b" doc position)
                 (let ((word (match-string 1 doc)))
                   (setq position (match-end 0))
                   (when (and (not (memq (intern word) arguments))
                              (not (member word lsp-bridge-doc-check--acronyms))
                              (not (memq (intern (downcase word)) lowered)))
                     (push (format "%s says %s" symbol word) found))))))))))
    (nreverse found)))

(let* ((missing (lsp-bridge-doc-check--missing-symbols))
       (arguments (lsp-bridge-doc-check--unknown-arguments)))
  (princ (format "doc references to missing symbols: %d\n" (length missing)))
  (dolist (line missing) (princ (format "  %s\n" line)))
  (princ (format "doc words that are not arguments (for information): %d\n"
                 (length arguments)))
  (dolist (line arguments) (princ (format "  %s\n" line)))
  (kill-emacs (if missing 1 0)))

;;; check-doc-references.el ends here
