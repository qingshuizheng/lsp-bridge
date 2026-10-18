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
      (should (= (acm-popup menu-max-length-cache) 0)))))

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

;;; The two behaviour options

(ert-deftest acm-test-cycle-next-p ()
  "The wrap decision of `acm-select-next' is off without the option."
  (with-temp-buffer
    (setq-local acm-candidates '(a b c)
                acm-menu-candidates '(a b c)
                acm-menu-offset 0
                acm-menu-index 2)
    (setq-local acm-menu-cycle nil)
    (should-not (acm--cycle-next-p))
    (setq-local acm-menu-cycle t)
    (should (acm--cycle-next-p))
    ;; only the last candidate of the whole list wraps, not the last of a page
    (setq-local acm-candidates '(a b c d e) acm-menu-candidates '(a b c))
    (should-not (acm--cycle-next-p))))

(ert-deftest acm-test-cycle-prev-p ()
  "The wrap decision of `acm-select-prev' is off without the option."
  (with-temp-buffer
    (setq-local acm-candidates '(a b c)
                acm-menu-candidates '(a b c)
                acm-menu-offset 0
                acm-menu-index 0)
    (setq-local acm-menu-cycle nil)
    (should-not (acm--cycle-prev-p))
    (setq-local acm-menu-cycle t)
    (should (acm--cycle-prev-p))
    (setq-local acm-menu-index 1)
    (should-not (acm--cycle-prev-p))))

(ert-deftest acm-test-select-next-uses-the-wrap-decision ()
  "`acm-select-next' wraps through `acm-select-first' when the decision says so."
  (with-temp-buffer
    (setq-local acm-candidates '(a b c)
                acm-menu-candidates '(a b c)
                acm-menu-offset 0
                acm-menu-index 2
                acm-menu-cycle t)
    (let (wrapped)
      (cl-letf (((symbol-function 'acm-select-first) (lambda () (setq wrapped t))))
        (acm-select-next))
      (should wrapped))))

(ert-deftest acm-test-select-prev-uses-the-wrap-decision ()
  "`acm-select-prev' wraps through `acm-select-last' when the decision says so."
  (with-temp-buffer
    (setq-local acm-candidates '(a b c)
                acm-menu-candidates '(a b c)
                acm-menu-offset 0
                acm-menu-index 0
                acm-menu-cycle t)
    (let (wrapped)
      (cl-letf (((symbol-function 'acm-select-last) (lambda () (setq wrapped t))))
        (acm-select-prev))
      (should wrapped))))

(ert-deftest acm-test-at-blank-position ()
  "The blank check covers the start of a line and whitespace before point."
  (with-temp-buffer
    (insert "foo\n\nbar baz")
    ;; note that `insert' leaves point at the end of the text, so the first
    ;; position has to be chosen explicitly
    (goto-char (point-min))
    (should (acm--at-blank-position-p))          ; beginning of a line
    (forward-char 1)
    (should-not (acm--at-blank-position-p))      ; inside a word
    (forward-line 1)
    (should (acm--at-blank-position-p))          ; beginning of an empty line
    (forward-line 1)
    (search-forward "baz")
    (should-not (acm--at-blank-position-p))      ; inside a word
    (goto-char (point-max))
    (insert " ")
    (should (acm--at-blank-position-p))          ; right after a space
    (insert "\t")
    (should (acm--at-blank-position-p))))        ; and after a tab

(ert-deftest acm-test-post-command-hides-on-blank ()
  "`acm--post-command' hides the menu only when the option is on."
  (with-temp-buffer
    (insert "foo")
    (goto-char (point-min))              ; the menu is up while point is at blank
    (let (hidden)
      (cl-letf (((symbol-function 'acm-frame-visible-p) (lambda (&rest _) t))
                ((symbol-function 'acm-hide) (lambda () (setq hidden t))))
        (setq-local acm-hide-on-blank t)
        (acm--post-command)
        (should hidden)
        (setq hidden nil)
        (setq-local acm-hide-on-blank nil)
        (acm--post-command)
        (should-not hidden)))))

(ert-deftest acm-test-hide-runs-its-hook ()
  "`acm-hide' tells whoever kept state for the menu that it is gone."
  (with-temp-buffer
    (let (called)
      (add-hook 'acm-hide-hook (lambda () (setq called t)) nil t)
      (acm-hide)
      (should called))))

(ert-deftest acm-test-hide-is-not-advised ()
  "lsp-bridge keeps its menu state through the hook, not through advice."
  (should-not (advice-member-p 'lsp-bridge--reset-completion-item-ticker 'acm-hide)))

(ert-deftest acm-test-kbd-quick-access-builds-the-sequence ()
  "The quick-access key of a modifier and a key is the sequence of both."
  (should (equal (acm-keymap--kbd-quick-access "M" "1") (kbd "M-1")))
  (should (equal (acm-keymap--kbd-quick-access "C" "a") (kbd "C-a"))))

(ert-deftest acm-test-icon-filepath-names-the-svg ()
  "An icon file is the collection and the name, under the icon directory."
  (require 'acm-icon)
  (let ((path (acm-icon-filepath "material" "check")))
    (should (string-suffix-p "material_check.svg" path))
    (should (file-name-absolute-p path))
    (should (string-prefix-p (file-name-as-directory acm-icon-dir) path))))

(ert-deftest acm-test-icon-color-becomes-hex ()
  "A colour Emacs knows is written as #rrggbb; one it does not is left alone."
  (require 'acm-icon)
  (should (equal (acm-icon-convert-to-svg-color "red") "#ff0000"))
  (should (equal (acm-icon-convert-to-svg-color "not-a-colour-at-all")
                 "not-a-colour-at-all")))

(ert-deftest acm-test-ui-override-looks-up-the-alist ()
  "A front end takes a function over by putting it in `acm-ui-overrides'."
  (let ((acm-ui-overrides '((acm-doc-hide . ignore))))
    (should (eq (acm-ui-override 'acm-doc-hide) 'ignore))
    (should-not (acm-ui-override 'acm-hide))))

(ert-deftest acm-test-ui-override-is-honoured ()
  "The function a front end left in the alist is the one that runs."
  (let* ((called nil)
         (acm-ui-overrides (list (cons 'acm-doc-hide (lambda () (setq called 'override))))))
    (cl-letf (((symbol-function 'acm-doc--hide) (lambda () (setq called 'default))))
      (acm-doc-hide))
    (should (eq called 'override))
    (setq called nil)
    (let ((acm-ui-overrides nil))
      (cl-letf (((symbol-function 'acm-doc--hide) (lambda () (setq called 'default))))
        (acm-doc-hide)))
    (should (eq called 'default))))

(ert-deftest acm-test-bind-quick-access-binds-the-keys ()
  "Every quick-access key gets the modifier of the platform."
  (let ((acm-quick-access-keys '("1" "2"))
        (keymap (make-sparse-keymap)))
    (cl-letf (((symbol-function 'acm-keymap--quick-access-modifier) (lambda () "M")))
      (acm-keymap--bind-quick-access keymap))
    (should (eq (lookup-key keymap (kbd "M-1")) #'acm-complete-quick-access))
    (should (eq (lookup-key keymap (kbd "M-2")) #'acm-complete-quick-access))))

(ert-deftest acm-test-bind-quick-access-keeps-what-is-there ()
  "A key the mode map already uses is left alone."
  (let ((acm-quick-access-keys '("1"))
        (keymap (make-sparse-keymap)))
    (define-key keymap (kbd "M-1") #'ignore)
    (cl-letf (((symbol-function 'acm-keymap--quick-access-modifier) (lambda () "M")))
      (acm-keymap--bind-quick-access keymap))
    (should (eq (lookup-key keymap (kbd "M-1")) #'ignore))))

(ert-deftest acm-test-complete-quick-access-selects-the-row ()
  "The row of the key that was pressed is the candidate that is completed."
  (with-temp-buffer
    (let (completed)
      (cl-letf (((symbol-function 'acm-complete) (lambda () (setq completed t))))
        (acm-complete-quick-access 2)
        (should (equal acm-menu-index 2))
        (should completed)
        (setq completed nil)
        (acm-complete-quick-access nil)
        (should-not completed)))))

(ert-deftest acm-test-quick-access-init-honours-the-option ()
  "The keys are bound once, and only while the option is on."
  (let ((acm-quick-access-init-p nil)
        (acm-quick-access-keys '("1"))
        (acm-mode-map (make-sparse-keymap)))
    (cl-letf (((symbol-function 'acm-keymap--quick-access-modifier) (lambda () "M")))
      (let ((acm-enable-quick-access nil))
        (acm-quick-access-init)
        (should-not acm-quick-access-init-p)
        (should-not (lookup-key acm-mode-map (kbd "M-1"))))
      (let ((acm-enable-quick-access t))
        (acm-quick-access-init)
        (should acm-quick-access-init-p)
        (should (eq (lookup-key acm-mode-map (kbd "M-1"))
                    #'acm-complete-quick-access))))))

(ert-deftest acm-test-hiding-the-documentation-cancels-its-timer ()
  "The pending markdown render of the documentation is cancelled and forgotten.
The timer lives in a session slot, so it is cancelled through the slot: passing it to
`acm-cancel-timer', which takes a variable, expanded to a `setq' on a form."
  (let ((stand-in 'acm-test-markdown-timer)
        cancelled)
    (acm-popup-set 'markdown-timer stand-in)
    (acm-popup-set 'markdown-doc "rendered")
    (cl-letf (((symbol-function 'acm-frame-hide-frame) (lambda (&rest _) nil))
              ((symbol-function 'cancel-timer) (lambda (timer) (setq cancelled timer))))
      (acm-doc--hide))
    (should (eq cancelled stand-in))
    (should-not (acm-popup markdown-timer))
    (should-not (acm-popup markdown-doc))))

(ert-deftest acm-test-taking-a-candidate-out-of-read-only-text ()
  "Taking a candidate writes over the token even where the text is read-only.
Telega marks the messages of a chat buffer read-only, and what a candidate
replaces is the token being completed; without the binding this signalled
`text-read-only', which is what came out of the post-command hook."
  (with-temp-buffer
    (insert "Bob: hello @bo")
    (add-text-properties (point-min) (point-max) '(read-only t))
    (acm-popup-set 'popup-point (- (point) 3))
    (cl-letf (((symbol-function 'acm-menu-current-candidate)
               (lambda () (list :label "@bob" :backend "not-registered"))))
      (acm-complete t)
      (should (equal (buffer-string) "Bob: hello @bob")))))

(ert-deftest acm-test-the-two-options-are-on-by-default ()
  "Cycling the menu and hiding it at a blank position are both the default.
They are what this fork wants out of the box; a configuration that set them is
left with nothing to set."
  (should (default-value 'acm-menu-cycle))
  (should (default-value 'acm-hide-on-blank)))

(provide 'acm-test)
;;; acm-test.el ends here
