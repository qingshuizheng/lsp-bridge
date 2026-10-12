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
;; The content helpers convert protocol positions with this one.
(require 'acm-backend-lsp)

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

(defun lsp-bridge-peek-test--symbol-names (line)
  "Return the text of the symbols `lsp-bridge-peek--search-symbols' finds for LINE."
  (mapcar (lambda (pair) (buffer-substring-no-properties (car pair) (cdr pair)))
          (lsp-bridge-peek--search-symbols line)))

(ert-deftest lsp-bridge-peek-test-search-symbols ()
  "The symbols of the next lines are found, as start and end pairs."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "alpha (beta gamma)\n")
    (goto-char (point-min))
    (should (equal (lsp-bridge-peek-test--symbol-names 1)
                   '("alpha" "beta" "gamma")))))

(ert-deftest lsp-bridge-peek-test-search-symbols-stops-at-the-line-asked-for ()
  "Only the line asked for is searched, and point is left where it was."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "first-line\nsecond-line\n")
    (goto-char (point-min))
    (should (equal (lsp-bridge-peek-test--symbol-names 1) '("first-line")))
    (should (equal (point) (point-min)))
    (goto-char (point-min))
    (should (equal (lsp-bridge-peek-test--symbol-names 2)
                   '("first-line" "second-line")))))

(ert-deftest lsp-bridge-peek-test-search-symbols-and-a-comment ()
  "What a comment holds is searched like any other text."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "code ; a-comment\n")
    (goto-char (point-min))
    ;; The docstring of the function says comments are jumped over; the search
    ;; walks by syntax, which does not know about them, so the comment is searched.
    (should (equal (lsp-bridge-peek-test--symbol-names 1) '("code" "a-comment")))))

(ert-deftest lsp-bridge-peek-test-make-border ()
  "The border is a newline carrying the border face."
  (let ((border (lsp-bridge-peek--make-border)))
    (should (equal border "\n"))
    (should (eq (get-text-property 0 'face border) 'lsp-bridge-peek-border-face))))

(defun lsp-bridge-peek-test--tree ()
  "Return a symbol tree with one symbol that has two references."
  (list (list "symbol"
              (list "/tmp/a.py" "/tmp/b.py")
              (list (list :line 0 :character 0)
                    (list :line 4 :character 2)))
        nil))

(ert-deftest lsp-bridge-peek-test-get-content ()
  "The content around a position is taken for the height asked for."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "line one\nline two\nline three\nline four\n")
    (setq-local lsp-bridge-peek-file-content-height 2)
    (should (equal (lsp-bridge-peek--get-content '(:line 1 :character 0) 0)
                   "line two\nline three\n"))))

(ert-deftest lsp-bridge-peek-test-get-content-highlights-the-symbol ()
  "The symbol at the position carries the highlight face in what is returned."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "first\nsecond\n")
    (setq-local lsp-bridge-peek-file-content-height 2)
    (let ((content (lsp-bridge-peek--get-content '(:line 1 :character 0) 0)))
      ;; The property holds the face itself, not a list of faces.
      (should (eq (get-text-property 0 'face content)
                  'lsp-bridge-peek--highlight-symbol-face)))))

(ert-deftest lsp-bridge-peek-test-displaying-list ()
  "The list names the first entry a definition and the rest references."
  (let ((lsp-bridge-peek-list-height 3)
        (lsp-bridge-peek-selected-symbol 0)
        (lsp-bridge-peek-chosen-displaying-list '(0 1 nil))
        (lsp-bridge-peek-symbol-tree (lsp-bridge-peek-test--tree)))
    (let ((text (substring-no-properties (lsp-bridge-peek--displaying-list))))
      (should (string-match-p "definition" text))
      (should (string-match-p "reference" text))
      (should (string-match-p "/tmp/a.py" text))
      (should (string-match-p "/tmp/b.py" text))
      ;; The line and character of the protocol are counted from one for people.
      (should (string-match-p "1:1" text))
      (should (string-match-p "5:3" text)))))

(provide 'lsp-bridge-peek-test)
;;; lsp-bridge-peek-test.el ends here
