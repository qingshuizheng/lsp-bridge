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

;; Read by the pulse library rather than passed to it.
(defvar pulse-delay)

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

(ert-deftest lsp-bridge-jump-test-flash-region-keeps-its-duration ()
  "The delay of the flash is bound for the pulse library to read.
The library reads `pulse-delay' itself rather than taking it as an argument, which
is why the binding is not an unused variable and why removing it changes how long
the flash lasts."
  (require 'pulse)
  (let ((lsp-bridge-flash-region-delay 0.75)
        seen)
    (cl-letf (((symbol-function 'pulse-momentary-highlight-region)
               (lambda (&rest _args) (setq seen pulse-delay))))
      (lsp-bridge-flash-region 1 2))
    (should (equal seen 0.75))))

(ert-deftest lsp-bridge-jump-test-mark-ring-is-recorded-and-pushed ()
  "The position of the old buffer is kept, and pushed onto the ring of the new one.
The two functions say they are used in pairs: the first in the buffer being left,
the second in the one jumped to."
  (let ((lsp-bridge-mark-ring nil))
    (with-temp-buffer
      (insert "one two three\n")
      (goto-char 5)
      (lsp-bridge--record-mark-ring)
      ;; The marker is of the buffer it was taken in, at the position it was taken.
      (should (equal (marker-position lsp-bridge-position-before-jump) 5))
      (should (eq (marker-buffer lsp-bridge-position-before-jump) (current-buffer))))
    (let ((recorded lsp-bridge-position-before-jump))
      (with-temp-buffer
        (lsp-bridge--set-mark-ring-in-new-buffer)
        ;; The new buffer's ring starts with what was recorded in the old one.
        (should (eq (car lsp-bridge-mark-ring) recorded))
        (should (eq (marker-buffer (car lsp-bridge-mark-ring)) (marker-buffer recorded)))))))

(ert-deftest lsp-bridge-jump-test-mark-ring-accumulates ()
  "A second jump pushes a second position in front of the first."
  (let ((lsp-bridge-mark-ring nil))
    (with-temp-buffer
      (insert "first\n")
      (goto-char 3)
      (lsp-bridge--record-mark-ring))
    (let ((first lsp-bridge-position-before-jump))
      (with-temp-buffer
        (lsp-bridge--set-mark-ring-in-new-buffer)
        (should (eq (car lsp-bridge-mark-ring) first))
        ;; A second jump is recorded and pushed in front of the first one.
        (goto-char 2)
        (lsp-bridge--record-mark-ring)
        (lsp-bridge--set-mark-ring-in-new-buffer)
        (should (equal (length lsp-bridge-mark-ring) 2))
        (should-not (eq (car lsp-bridge-mark-ring) first))
        (should (eq (nth 1 lsp-bridge-mark-ring) first))))))

(defun lsp-bridge-jump-test--ranges-of (text needle)
  "Return the ranges of every NEEDLE in TEXT, as the live rename is given them."
  (let ((ranges nil) (start 0))
    (while (string-match (regexp-quote needle) text start)
      (push (cons (1+ (match-beginning 0)) (1+ (match-end 0))) ranges)
      (setq start (match-end 0)))
    (nreverse ranges)))

(defmacro lsp-bridge-jump-test--with-live-rename (text needle &rest body)
  "Run BODY in a buffer of TEXT with a live rename over every NEEDLE."
  `(with-temp-buffer
     (insert ,text)
     ;; Point has to be on the symbol, or the live rename has nothing to work on.
     (goto-char (point-min))
     (lsp-bridge-live-rename--setup (lsp-bridge-jump-test--ranges-of ,text ,needle))
     (unwind-protect (progn ,@body)
       (when lsp-bridge-live-rename-mode
         (lsp-bridge-live-rename-abort)))))

(ert-deftest lsp-bridge-jump-test-live-rename-changes-nothing-at-first ()
  "Starting a live rename highlights, and does not change one character."
  (lsp-bridge-jump-test--with-live-rename "one two one\n" "one"
    (should (equal (buffer-string) "one two one\n"))
    (should lsp-bridge-live-rename-mode)
    ;; One overlay per occurrence, and none of them shows anything yet.
    (should (equal (length lsp-bridge-live-rename--overlays) 2))
    ;; The occurrence at point shows nothing over it - it is there to be typed in -
    ;; and the others show the name the symbol still has.
    (should (seq-every-p (lambda (overlay)
                           (if (overlay-get overlay 'lsp-bridge-live-rename-active)
                               (null (overlay-get overlay 'display))
                             (equal (overlay-get overlay 'display) "one")))
                         lsp-bridge-live-rename--overlays))
    (should lsp-bridge-live-rename--pending-delete)))

(ert-deftest lsp-bridge-jump-test-live-rename-removes-on-the-first-typing ()
  "The first character typed takes the place of the highlighted occurrence."
  (lsp-bridge-jump-test--with-live-rename "one two one\n" "one"
    ;; Point is left at the end of the occurrence at point.
    (should (equal (point) 4))
    (insert "X")
    (should (equal (buffer-string) "X two one\n"))
    (should-not lsp-bridge-live-rename--pending-delete)
    ;; The other occurrence now shows what was typed.
    (let ((other (seq-find (lambda (overlay) (overlay-get overlay 'lsp-bridge-live-rename))
                           lsp-bridge-live-rename--overlays)))
      (should (equal (overlay-get other 'display) "X")))))

(ert-deftest lsp-bridge-jump-test-live-rename-abort-restores-what-was-typed ()
  "Giving up puts the occurrence that was typed over back as it was."
  (lsp-bridge-jump-test--with-live-rename "one two one\n" "one"
    (insert "X")
    (should (equal (buffer-string) "X two one\n"))
    (lsp-bridge-live-rename-abort)
    (should (equal (buffer-string) "one two one\n"))
    (should-not lsp-bridge-live-rename-mode)
    (should-not lsp-bridge-live-rename--overlays)))

(ert-deftest lsp-bridge-jump-test-live-rename-abort-without-typing ()
  "Giving up before anything was typed leaves the buffer as it was."
  (lsp-bridge-jump-test--with-live-rename "one two one\n" "one"
    (lsp-bridge-live-rename-abort)
    (should (equal (buffer-string) "one two one\n"))
    (should-not lsp-bridge-live-rename-mode)))

(ert-deftest lsp-bridge-jump-test-live-rename-typing-a-whole-name ()
  "A name typed character by character replaces the occurrence as it goes."
  (lsp-bridge-jump-test--with-live-rename "one two one\n" "one"
    (insert "a")
    (insert "b")
    (insert "c")
    (should (equal (buffer-string) "abc two one\n"))
    (lsp-bridge-live-rename-abort)
    (should (equal (buffer-string) "one two one\n"))))

(ert-deftest lsp-bridge-jump-test-the-highlight-faces-follow-the-theme ()
  "Neither face names a colour of its own, so the theme decides how they look."
  (should (equal (face-attribute 'lsp-bridge-live-rename-face :inherit)
                 'secondary-selection))
  (should (equal (face-attribute 'lsp-bridge-document-highlight-face :inherit)
                 'highlight))
  ;; A fixed background is what made the rename hard to see in a light theme, and
  ;; which colour the themes give these faces is theirs to decide, so what is asked
  ;; here is the inheritance alone.
  )

(provide 'lsp-bridge-jump-test)
;;; lsp-bridge-jump-test.el ends here
