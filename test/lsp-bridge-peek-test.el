;;; lsp-bridge-peek-test.el --- Colour arithmetic and ace keys  -*- lexical-binding: t; -*-

;;; Commentary:
;; The peek window paints its borders and its symbol list itself, which needs two
;; things that are pure: the sRGB transfer functions, used to blend two colours in
;; a space where the mixture feels linear to the eye, and the ace key sequences
;; that are handed out to the symbols of the list and consumed one key at a time.
;; Both are arithmetic and list work, so both are asked here.  What is not here is
;; the window itself - its overlays, its scrolling and its keymap - which needs a
;; real frame to mean anything.

;;; Code:

(require 'ert)
(require 'lsp-bridge-peek)

(defun lsp-bridge-peek-test--close-to (expected actual)
  "Return non-nil when ACTUAL is EXPECTED within the tolerance of a colour."
  (< (abs (- expected actual)) 1e-6))

(ert-deftest lsp-bridge-peek-test-srgb-transfer-functions ()
  "The two transfer functions are inverses of each other."
  (should (lsp-bridge-peek-test--close-to 0.0 (lsp-bridge--color-srgb-to-rgb 0.0)))
  (should (lsp-bridge-peek-test--close-to 1.0 (lsp-bridge--color-srgb-to-rgb 1.0)))
  (should (lsp-bridge-peek-test--close-to 1.0 (lsp-bridge--color-rgb-to-srgb 1.0)))
  (dolist (component '(0.0 0.02 0.03928 0.04 0.2 0.5 0.75 1.0))
    (should (lsp-bridge-peek-test--close-to
             component
             (lsp-bridge--color-rgb-to-srgb (lsp-bridge--color-srgb-to-rgb component))))))

(ert-deftest lsp-bridge-peek-test-srgb-to-rgb-is-linear-at-the-bottom ()
  "A dark component is divided rather than raised to a power."
  (should (lsp-bridge-peek-test--close-to (/ 0.03928 12.92)
                                          (lsp-bridge--color-srgb-to-rgb 0.03928)))
  (should (lsp-bridge-peek-test--close-to
           (* 0.003 12.92)
           (lsp-bridge--color-rgb-to-srgb 0.003))))

(ert-deftest lsp-bridge-peek-test-blend-takes-the-ends-whole ()
  "A blend of one is the first colour, of nought the second."
  (should (equal (lsp-bridge--color-blend "#ff0000" "#0000ff" 1.0) "#ff0000"))
  (should (equal (lsp-bridge--color-blend "#ff0000" "#0000ff" 0.0) "#0000ff"))
  (should (equal (lsp-bridge--color-blend "#ff0000" "#ff0000" 0.5) "#ff0000")))

(ert-deftest lsp-bridge-peek-test-blend-is-between ()
  "A blend in the middle is neither of the two colours."
  (let ((blended (lsp-bridge--color-blend "#000000" "#ffffff" 0.5)))
    (should (string-match-p "\\`#[0-9a-f]\\{6\\}\\'" blended))
    (should-not (member blended '("#000000" "#ffffff")))))

(ert-deftest lsp-bridge-peek-test-add-face-keeps-the-surroundings-out ()
  "The face is added, and the default face appended after it."
  (let ((string (lsp-bridge--add-face "text" 'bold)))
    (should (equal string "text"))
    (let ((face (get-text-property 0 'face string)))
      (should (member 'bold face))
      ;; The default face is appended, so that the surroundings cannot win.
      (should (eq (car (last face)) 'default)))))

(ert-deftest lsp-bridge-peek-test-ace-key-sequences ()
  "Every symbol of the list gets a sequence of its own."
  (let ((lsp-bridge-peek-ace-keys '(?a ?s ?d)))
    (should-not (lsp-bridge-peek--ace-key-seqs 0))
    (should (equal (lsp-bridge-peek--ace-key-seqs 1) '((?a))))
    (should (equal (lsp-bridge-peek--ace-key-seqs 3) '((?a) (?s) (?d))))
    (let* ((seqs (lsp-bridge-peek--ace-key-seqs 9))
           (strings (mapcar (lambda (seq) (apply #'string seq)) seqs)))
      (should (equal (length seqs) 9))
      (should (equal (length (delete-dups (copy-sequence strings))) 9))
      (should (equal (length (car seqs)) 2)))))

(ert-deftest lsp-bridge-peek-test-ace-keys-have-to-be-sane ()
  "A broken `lsp-bridge-peek-ace-keys' is refused instead of misused."
  (let ((lsp-bridge-peek-ace-keys '(?a ?a)))
    (should-error (lsp-bridge-peek--ace-key-seqs 2) :type 'user-error))
  (let ((lsp-bridge-peek-ace-keys '(?a "s")))
    (should-error (lsp-bridge-peek--ace-key-seqs 2) :type 'user-error)))

(ert-deftest lsp-bridge-peek-test-pop-ace-key-seqs ()
  "A key keeps what it starts, drops the rest, and reports the last one."
  (let ((seqs (list (list ?a ?a) (list ?a ?s) (list ?s))))
    ;; Pressing a drops the third and shortens the first two.
    (should (equal (lsp-bridge-peek--pop-ace-key-seqs seqs ?a)
                   (list (list ?a) (list ?s) nil)))
    ;; Pressing a key that starts nothing of what is left changes nothing.
    (should (equal (lsp-bridge-peek--pop-ace-key-seqs (list (list ?a) (list ?s)) ?z)
                   (list (list ?a) (list ?s))))
    ;; The last one left is answered by its index rather than by the list.
    (should (equal (lsp-bridge-peek--pop-ace-key-seqs (list nil (list ?s)) ?s) 1))))

(provide 'lsp-bridge-peek-test)
;;; lsp-bridge-peek-test.el ends here
