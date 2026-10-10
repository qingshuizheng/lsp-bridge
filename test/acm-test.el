;;; acm-test.el --- Head-less tests for the completion menu  -*- lexical-binding: t; -*-

;;; Commentary:

;; The completion menu is a child-frame UI, and in batch mode `posn-at-point'
;; returns nil, so `acm-update' stops before it touches a frame.  That is what
;; makes the state machine testable without an interactive Emacs: these tests
;; cover the candidate helpers, the input prefix, the predicates that decide
;; whether the menu may pop up, the quick-access key builder and the
;; update/hide cycle -- the parts the popup refactor has to keep working.
;;
;; Run them with:
;;
;;   Emacs -Q --batch -L . -L acm -L test -l test/acm-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; or through scripts/check-test.sh, which also runs the Python tests.

;;; Code:

(require 'ert)
(require 'acm)
(require 'lsp-bridge)

(defun acm-test--candidate (label &optional backend annotation)
  "Build a candidate plist with LABEL, BACKEND and ANNOTATION."
  (list :label label
        :backend (or backend "lsp")
        :annotation annotation))

(defun acm-test--labels (candidates)
  "Return the labels of CANDIDATES."
  (mapcar (lambda (candidate) (plist-get candidate :label)) candidates))

(defmacro acm-test--with-buffer (mode text &rest body)
  "Run BODY in a temporary buffer in MODE, holding TEXT with point at the end."
  (declare (indent 2))
  `(with-temp-buffer
     (funcall ,mode)
     (insert ,text)
     (goto-char (point-max))
     ,@body))

;;; Candidate helpers

(ert-deftest acm-test-candidate-fuzzy-search ()
  "The matcher is `acm-candidate-match-function' and ignores case."
  (let ((acm-candidate-match-function 'regexp-quote))
    (should (acm-candidate-fuzzy-search "foo" "my-foobar"))
    (should (acm-candidate-fuzzy-search "FOO" "my-foobar"))
    (should-not (acm-candidate-fuzzy-search "xyz" "my-foobar")))
  ;; Without a matcher every candidate matches.
  (let ((acm-candidate-match-function nil))
    (should (acm-candidate-fuzzy-search "anything" "whatever"))))

(ert-deftest acm-test-candidate-sort-by-prefix ()
  "Candidates starting with the keyword come first, shortest label first."
  (let ((sorted (acm-candidate-sort-by-prefix
                 "ab" (list (acm-test--candidate "zzz")
                            (acm-test--candidate "abcd")
                            (acm-test--candidate "ab")))))
    (should (equal (acm-test--labels sorted) '("ab" "abcd" "zzz"))))
  ;; The secondary comparison has to look at the label, not at the plist: with
  ;; an empty keyword every candidate "starts with" it, so the order is decided
  ;; by label length alone.
  (let ((sorted (acm-candidate-sort-by-prefix
                 "" (list (acm-test--candidate "zzz")
                          (acm-test--candidate "ab")
                          (acm-test--candidate "abcd")))))
    (should (equal (acm-test--labels sorted) '("ab" "zzz" "abcd"))))
  ;; Nothing to sort: an empty candidate list stays nil, and without a keyword
  ;; the list is returned as it is.
  (should-not (acm-candidate-sort-by-prefix "ab" nil))
  (should (equal (acm-test--labels
                  (acm-candidate-sort-by-prefix
                   nil (list (acm-test--candidate "b") (acm-test--candidate "a"))))
                 '("b" "a"))))

(ert-deftest acm-test-remove-duplicate-candidates ()
  "Candidates whose label is in LSP-LABELS are dropped."
  (let ((candidates (list (acm-test--candidate "a")
                          (acm-test--candidate "b")
                          (acm-test--candidate "c"))))
    (should (equal (acm-test--labels
                    (acm-remove-duplicate-candidates candidates '("b")))
                   '("a" "c")))
    (should (equal (acm-test--labels
                    (acm-remove-duplicate-candidates candidates nil))
                   '("a" "b" "c")))))

(ert-deftest acm-test-menu-index-info ()
  "The candidate identity joins the label and the backend."
  (should (equal (acm-menu-index-info (acm-test--candidate "foo" "ctags"))
                 "foo###ctags"))
  (should (equal (acm-menu-index-info (acm-test--candidate "foo"))
                 "foo###lsp")))

;;; Input prefix

(ert-deftest acm-test-get-input-prefix ()
  "The prefix at point is the symbol part of the text before it."
  (acm-test--with-buffer #'emacs-lisp-mode "abc"
    (should (equal (acm-get-input-prefix) "abc"))
    (should (equal (buffer-substring-no-properties
                    (car (acm-get-input-prefix-bound)) (point))
                   "abc")))
  ;; The "ascii" input bound style keeps only the ASCII tail of the symbol,
  ;; which is what makes mixed CJK/ASCII identifiers completable.
  (acm-test--with-buffer #'emacs-lisp-mode "中文abc"
    (should (equal (acm-get-input-prefix) "abc")))
  ;; No prefix at all.
  (acm-test--with-buffer #'emacs-lisp-mode " "
    (should (equal (acm-get-input-prefix) ""))))

;;; The predicates that gate the popup

(ert-deftest acm-test-not-match-hide-characters ()
  "A character from `lsp-bridge-completion-hide-characters' hides the menu."
  (acm-test--with-buffer #'emacs-lisp-mode "("
    (should-not (lsp-bridge--not-match-hide-characters)))
  (acm-test--with-buffer #'emacs-lisp-mode "a"
    (should (lsp-bridge--not-match-hide-characters))))

(ert-deftest acm-test-not-match-stop-commands ()
  "A completion stop command or a stopped last change hides the menu."
  (acm-test--with-buffer #'emacs-lisp-mode "a"
    (let ((lsp-bridge-last-change-command 'self-insert-command)
          (last-command 'self-insert-command))
      (should (lsp-bridge--not-match-stop-commands)))
    (let ((lsp-bridge-last-change-command 'yank)
          (last-command 'self-insert-command))
      (should-not (lsp-bridge--not-match-stop-commands)))
    (let ((lsp-bridge-last-change-command 'self-insert-command)
          (last-command 'query-replace))
      (should-not (lsp-bridge--not-match-stop-commands)))))

(ert-deftest acm-test-not-in-string ()
  "Completion inside a string is refused unless the user allowed it.
`acm-in-string-p' answers nil at the end of the buffer, so point has to
sit inside the string for this test."
  (acm-test--with-buffer #'emacs-lisp-mode "\"abc\" def"
    (goto-char (point-min))
    (forward-char 2)
    (should (acm-in-string-p))
    (should-not (lsp-bridge--not-in-string)))
  (acm-test--with-buffer #'emacs-lisp-mode "\"abc\" def"
    (should (lsp-bridge--not-in-string)))
  (acm-test--with-buffer #'emacs-lisp-mode "\"abc\" def"
    (goto-char (point-min))
    (forward-char 2)
    (let ((lsp-bridge-enable-completion-in-string t))
      (should (lsp-bridge--not-in-string)))))

;;; Quick access and icons

(ert-deftest acm-test-quick-access-modifier ()
  "The modifier option is turned into its key string."
  (let ((acm-quick-access-modifier 'meta))
    (should (equal (acm-keymap--quick-access-modifier) "M")))
  (let ((acm-quick-access-modifier 'super))
    (should (equal (acm-keymap--quick-access-modifier) "s")))
  (let ((acm-quick-access-modifier 'hyper))
    (should (equal (acm-keymap--quick-access-modifier) "H"))))

(ert-deftest acm-test-quick-access-key-sequence ()
  "The quick-access sequence is the modifier plus the configured key."
  (should (equal (acm-keymap--kbd-quick-access "M" "1") (kbd "M-1")))
  (should (equal (acm-keymap--kbd-quick-access "s" "0") (kbd "s-0"))))

(ert-deftest acm-test-icon-filepath ()
  "An icon lives in the icon directory, named COLLECTION_NAME.svg."
  (let ((acm-icon-dir "/tmp/acm-icons/"))
    (should (equal (acm-icon-filepath "material" "text")
                   "/tmp/acm-icons/material_text.svg"))))

;;; The update / hide cycle

(ert-deftest acm-test-update-records-candidates ()
  "`acm-update' records the candidate state even without a frame."
  (acm-test--with-buffer #'emacs-lisp-mode "abc"
    (let ((acm-menu-length 10)
          (acm-enable-quick-access nil))
      (acm-update (list (acm-test--candidate "abcd")
                        (acm-test--candidate "abcde")))
      (should (equal (acm-test--labels acm-menu-candidates) '("abcd" "abcde")))
      (should (equal (acm-test--labels acm-candidates) '("abcd" "abcde")))
      (should (= acm-menu-index 0))
      (should (= acm-menu-offset 0))
      (should (bound-and-true-p acm-mode))
      ;; The menu itself only shows `acm-menu-length' candidates.
      (let ((acm-menu-length 1))
        (acm-update (list (acm-test--candidate "abcd")
                          (acm-test--candidate "abcde")))
        (should (equal (acm-test--labels acm-menu-candidates) '("abcd"))))
      (acm-hide)
      (should-not (bound-and-true-p acm-mode))
      (should (= acm-menu-max-length-cache 0)))))

(ert-deftest acm-test-update-hides-on-single-exact-match ()
  "One candidate identical to the prefix hides the menu, templates excepted."
  (acm-test--with-buffer #'emacs-lisp-mode "abcd"
    (let ((acm-enable-quick-access nil))
      (acm-update (list (acm-test--candidate "abcd")))
      (should-not acm-candidates)
      ;; A template that matches exactly stays visible.
      (acm-update (list (acm-test--candidate "abcd" "yas" "Snippet")))
      (should (equal (acm-test--labels acm-candidates) '("abcd"))))))

(ert-deftest acm-test-update-keeps-selection ()
  "Re-selecting the same candidate keeps the menu index on it."
  (acm-test--with-buffer #'emacs-lisp-mode "abc"
    (let ((acm-enable-quick-access nil))
      (acm-update (list (acm-test--candidate "abcd")
                        (acm-test--candidate "abcde")))
      ;; Pretend the user moved to the second candidate, then a new backend
      ;; returns the same list: the selection must stay on "abcde".
      (setq-local acm-menu-index 1)
      (acm-update (list (acm-test--candidate "abcd")
                        (acm-test--candidate "abcde")))
      (should (= acm-menu-index 1))
      (should (equal (plist-get (acm-menu-current-candidate) :label) "abcde")))))

(provide 'acm-test)
;;; acm-test.el ends here
