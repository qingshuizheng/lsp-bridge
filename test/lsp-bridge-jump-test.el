;;; lsp-bridge-jump-test.el --- Finding textual occurrences of a symbol  -*- lexical-binding: t; -*-

;;; Commentary:
;; The live rename works on the occurrences of a symbol inside the buffer it
;; started in: the one at point is typed into, the others are shown through
;; overlays.  Which occurrences those are is a question about text and syntax -
;; whole words only, case significant, at most a thousand of them - and that is
;; what is asked here.  The rest of the live rename needs the buffer to be set up
;; with markers and overlays and a hook from the server, which is not text.

;;; Code:

(require 'ert)
(require 'lsp-bridge)
(require 'lsp-bridge-jump)

(ert-deftest lsp-bridge-jump-test-symbol-char-p ()
  "What counts as a symbol character is the syntax table of the buffer."
  (with-temp-buffer
    (emacs-lisp-mode)
    (should (lsp-bridge-live-rename--symbol-char-p ?a))
    (should (lsp-bridge-live-rename--symbol-char-p ?-))
    ;; Emacs Lisp makes a dot a symbol constituent as well.
    (should (lsp-bridge-live-rename--symbol-char-p ?.))
    (should-not (lsp-bridge-live-rename--symbol-char-p ?\s)))
  (with-temp-buffer
    ;; The standard syntax table makes a dot punctuation, where Emacs Lisp makes
    ;; it a symbol constituent, which is what shows the answer comes from there.
    (should-not (lsp-bridge-live-rename--symbol-char-p ?.))))

(ert-deftest lsp-bridge-jump-test-textual-ranges-are-whole-words ()
  "Only whole words are found, and the case has to be the same."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "foo foobar barfoo foo Foo foo\n")
    (let ((ranges (lsp-bridge-live-rename--textual-ranges "foo")))
      (should (equal (mapcar (lambda (range)
                               (buffer-substring-no-properties (car range) (cdr range)))
                             ranges)
                     '("foo" "foo" "foo")))
      (should (equal (mapcar #'car ranges) '(1 19 27))))))

(ert-deftest lsp-bridge-jump-test-textual-ranges-stop-at-a-thousand ()
  "At most a thousand occurrences are collected, as the docstring says."
  (with-temp-buffer
    (emacs-lisp-mode)
    (dotimes (_ 1001) (insert "sym "))
    (should (equal (length (lsp-bridge-live-rename--textual-ranges "sym")) 1000))))

(provide 'lsp-bridge-jump-test)
;;; lsp-bridge-jump-test.el ends here
