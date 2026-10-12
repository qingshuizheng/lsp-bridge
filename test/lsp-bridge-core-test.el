;;; lsp-bridge-core-test.el --- Tests for the shared core helpers  -*- lexical-binding: t; -*-

;;; Commentary:

;; The helpers here are used by several files of the package, so they are tested
;; on their own.  `lsp-bridge-command-name' is the one that matters for
;; robustness: naming a command must never print the command object, because
;; `this-command'/`last-command' can hold a closure with an arbitrarily large
;; environment, and `prin1' on one of those builds its string in C without
;; checking for quit - Emacs then ignores C-g and has to be killed.
;;
;;   Emacs -Q --batch -L . -L acm -L test -l test/lsp-bridge-core-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'org)
(require 'lsp-bridge-core)
(require 'acm-backend-lsp)
;; Defined by the lsp backend; declared here for the compiler.
(defvar acm-backend-lsp-filepath)

(defun lsp-bridge-core-test--closure-with-big-environment ()
  "Return a closure whose environment is large enough to be worth not printing."
  (let ((payload (make-list 200000 :lsp-bridge-test-payload)))
    (lambda () payload)))

(ert-deftest lsp-bridge-core-test-command-name-of-a-symbol ()
  "A symbol command is named by its symbol name."
  (should (equal (lsp-bridge-command-name 'self-insert-command) "self-insert-command"))
  (should (equal (lsp-bridge-command-name 'acm-complete) "acm-complete")))

(ert-deftest lsp-bridge-core-test-command-name-defaults-to-this-command ()
  "Without an argument the helper names `this-command'."
  (let ((this-command 'org-self-insert-command))
    (should (equal (lsp-bridge-command-name) "org-self-insert-command")))
  ;; `last-command' is the other variable that carries a command object.
  (let ((last-command 'acm-hide))
    (should (equal (lsp-bridge-command-name last-command) "acm-hide"))))

(ert-deftest lsp-bridge-core-test-command-name-never-prints-the-object ()
  "A closure command is named by its type, not printed.
This is the regression test for the hang: printing the closure below would
produce a string holding all of its environment, and the point is that the
helper must not do that."
  (let ((name (lsp-bridge-command-name (lsp-bridge-core-test--closure-with-big-environment))))
    (should (stringp name))
    (should (< (length name) 40))
    (should-not (string-match-p "payload" name))))

(ert-deftest lsp-bridge-core-test-command-name-of-other-objects ()
  "Anything that is not a symbol is named by its type."
  (should (equal (lsp-bridge-command-name [1 2 3]) "vector"))
  (should (equal (lsp-bridge-command-name "a string") "string"))
  ;; nil means \"no command given\", so this-command is used, which is what the
  ;; monitors rely on when they call the helper without an argument.
  (let ((this-command 'some-command))
    (should (equal (lsp-bridge-command-name nil) "some-command"))))

(ert-deftest lsp-bridge-core-test-the-column-is-counted-in-characters ()
  "The column is how many characters stand between the line start and point.
A character that takes two bytes of UTF-16 still counts once, which is what the
server expects: it asks for positions in the text, not in its encoding."
  (with-temp-buffer
    (insert "abc def\n")
    (goto-char 4)
    (should (equal (lsp-bridge--calculate-column) 3))
    (insert "\u4f60\u597d")
    (should (equal (lsp-bridge--calculate-column) 5))))

(ert-deftest lsp-bridge-core-test-org-temporary-buffers-are-known ()
  "The buffers org makes for itself are not buffers lsp-bridge should start in."
  (dolist (name '("*org-src-fontification:emacs-lisp*" "*Org Src x[*]" "x.org-src-babel" "*Capture*"))
    (with-temp-buffer
      (rename-buffer name t)
      (should (lsp-bridge-is-org-temp-buffer-p))))
  (with-temp-buffer
    (rename-buffer "a normal buffer" t)
    (should-not (lsp-bridge-is-org-temp-buffer-p))))

(ert-deftest lsp-bridge-core-test-the-position-outside-an-org-block ()
  "A buffer that is not an org source block gives the position it would anywhere."
  (with-temp-buffer
    (insert "one two\nthree\n")
    (goto-char 10)
    (should (equal (lsp-bridge--position-in-org) (lsp-bridge--position)))))

(ert-deftest lsp-bridge-core-test-the-rename-advisor-tells-the-server ()
  "Renaming the file of the buffer asks the server and updates the buffer.
The advisor is kept: it is one of the two advices on commands of Emacs itself."
  (with-temp-buffer
    (setq buffer-file-name (expand-file-name "/tmp/old-name.el"))
    ;; Both are read as special variables by the advisor, so they are set rather
    ;; than bound: a lexical binding is invisible to it.
    (let (calls renamed (was-mode (bound-and-true-p lsp-bridge-mode)))
      (setq-local lsp-bridge-mode t)
      (setq acm-backend-lsp-filepath "/tmp/old-name.el")
      (cl-letf (((symbol-function 'lsp-bridge-call-file-api)
                 (lambda (method &rest args) (push (cons method args) calls)))
                ((symbol-function 'lsp-bridge-call-async)
                 (lambda (method &rest args) (push (cons method args) calls)))
                ((symbol-function 'lsp-bridge-is-remote-file) (lambda () nil))
                ((symbol-function 'set-visited-file-name)
                 (lambda (name &rest _) (setq renamed name))))
        ;; `rename-file' takes the old name and then the new one, which is how the
        ;; advice sees them: the old one is what says whether this buffer is it.
        (lsp-bridge--rename-file-advisor #'ignore "/tmp/old-name.el" "/tmp/new-name.el"))
      (should (equal renamed "/tmp/new-name.el"))
      (should (equal (assoc "rename_file" calls) '("rename_file" "/tmp/new-name.el")))
      (should (assoc "close_file" calls))
      ;; Renaming some other file is not this buffer's business.
      (setq calls nil)
      (lsp-bridge--rename-file-advisor #'ignore "/tmp/other.el" "/tmp/elsewhere.el")
      (should-not calls)
      (setq-local lsp-bridge-mode was-mode))))

(ert-deftest lsp-bridge-core-test-the-revert-advisor-flags-the-revert ()
  "The flag is set for as long as the revert runs, and cleared after it."
  (with-temp-buffer
    (let (cleaned during)
      (cl-letf (((symbol-function 'lsp-bridge--cleanup) (lambda () (setq cleaned t))))
        (lsp-bridge--revert-buffer-advisor
         (lambda (&rest _) (setq during lsp-bridge-revert-buffer-flag))))
      (should cleaned)
      (should during)
      (should-not (bound-and-true-p lsp-bridge-revert-buffer-flag)))))

(provide 'lsp-bridge-core-test)
;;; lsp-bridge-core-test.el ends here
