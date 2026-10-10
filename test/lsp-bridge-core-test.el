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
(require 'lsp-bridge-core)

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

(provide 'lsp-bridge-core-test)
;;; lsp-bridge-core-test.el ends here
