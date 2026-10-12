;;; lsp-bridge-live-rename-test.el --- Live rename behaviour  -*- lexical-binding: t; -*-

;;; Commentary:
;; The live rename is driven here without a language server: the occurrences the
;; server would report are handed to the setup directly, and the file api is
;; stubbed, so what is checked is the behaviour the feature owns - the text of the
;; buffer stays as the server knows it while the occurrences follow along on
;; screen, and what is sent back on commit is the rename of the symbol.

;;; Code:

(require 'ert)
(require 'lsp-bridge)
(require 'lsp-bridge-workspace)
(require 'lsp-bridge-jump)

(defmacro lsp-bridge-live-rename-test--with-buffer (text ranges &rest body)
  "Run BODY in a buffer with TEXT, occurrences RANGES and a live rename set up."
  (declare (indent 2))
  `(with-temp-buffer
     (insert ,text)
     (goto-char (point-min))
     (let ((lsp-bridge-live-rename--pending nil))
       (lsp-bridge-live-rename--setup ,ranges)
       ,@body)))

(defun lsp-bridge-live-rename-test--displayed ()
  "Return the name the occurrences that are not edited display."
  (let (names)
    (dolist (overlay lsp-bridge-live-rename--overlays)
      (when (overlay-get overlay 'lsp-bridge-live-rename)
        (push (overlay-get overlay 'display) names)))
    (nreverse names)))

(ert-deftest lsp-bridge-live-rename-test-typing-leaves-the-buffer-alone ()
  "The buffer keeps its text; only the screen follows the new name."
  (lsp-bridge-live-rename-test--with-buffer
      "foo bar foo foo" '((1 . 4) (9 . 12) (13 . 16))
    (insert "renamed")
    (should (equal "renamed" lsp-bridge-live-rename--name))
    (should (equal '("renamed" "renamed") (lsp-bridge-live-rename-test--displayed)))
    ;; only the occurrence being typed in changed, the other two kept their text
    (should (equal "renamed bar foo foo" (buffer-string)))))

(ert-deftest lsp-bridge-live-rename-test-commit-asks-the-server ()
  "Commit puts the symbol back and sends the rename from it."
  (lsp-bridge-live-rename-test--with-buffer
      "foo bar foo" '((1 . 4) (9 . 12))
    (insert "renamed")
    (let (called)
      (cl-letf (((symbol-function 'lsp-bridge-call-file-api)
                 (lambda (&rest args) (setq called args)))
                ((symbol-function 'lsp-bridge--position)
                 (lambda () (list :line 0 :character 0))))
        (lsp-bridge-live-rename-commit))
      (should (equal '("rename" (:line 0 :character 0) "renamed") called))
      (should (equal "foo bar foo" (buffer-string)))
      (should-not lsp-bridge-live-rename-mode)
      (should-not lsp-bridge-live-rename--overlays))))

(ert-deftest lsp-bridge-live-rename-test-abort-asks-nothing ()
  "Abort puts the symbol back and never reaches the server."
  (lsp-bridge-live-rename-test--with-buffer
      "foo bar foo" '((1 . 4) (9 . 12))
    (insert "renamed")
    (let (called)
      (cl-letf (((symbol-function 'lsp-bridge-call-file-api)
                 (lambda (&rest args) (setq called args))))
        (lsp-bridge-live-rename-abort))
      (should-not called)
      (should (equal "foo bar foo" (buffer-string)))
      (should-not lsp-bridge-live-rename--overlays))))

(ert-deftest lsp-bridge-live-rename-test-empty-name-is-no-rename ()
  "An empty new name leaves everything as it was."
  (lsp-bridge-live-rename-test--with-buffer
      "foo bar foo" '((1 . 4) (9 . 12))
    (let (called)
      (cl-letf (((symbol-function 'lsp-bridge-call-file-api)
                 (lambda (&rest args) (setq called args))))
        (lsp-bridge-live-rename-commit))
      (should-not called)
      (should (equal "foo bar foo" (buffer-string))))))

(ert-deftest lsp-bridge-live-rename-test-highlights-start-the-session ()
  "The document highlight answer is what starts the live rename."
  (with-temp-buffer
    (insert "foo bar foo")
    (goto-char (point-min))
    (let ((lsp-bridge-live-rename--pending t))
      (lsp-bridge-live-rename--render-hook
       (list (list :range (list :start (list :line 0 :character 0)
                                :end (list :line 0 :character 3)))
             (list :range (list :start (list :line 0 :character 8)
                                :end (list :line 0 :character 11)))))
      (should lsp-bridge-live-rename-mode)
      (should (equal 2 (length lsp-bridge-live-rename--overlays)))
      (should-not lsp-bridge-live-rename--pending)
      ;; the occurrence at point is the one emptied for typing
      (should (equal " bar foo" (buffer-string)))
      (lsp-bridge-live-rename-abort)
      (should (equal "foo bar foo" (buffer-string))))))

(ert-deftest lsp-bridge-live-rename-test-delegates-without-a-server ()
  "Without a server the buffer-local rename does the work, in both commands."
  (with-temp-buffer
    (insert "foo bar foo")
    (goto-char (point-min))
    (let ((lsp-bridge-mode t)
          called)
      (cl-letf (((symbol-function 'lsp-bridge-has-lsp-server-p) (lambda () nil))
                ((symbol-function 'symbol-overlay-rename) (lambda () (interactive) (setq called t))))
        (call-interactively #'lsp-bridge-rename)
        (should called)
        (setq called nil)
        (call-interactively #'lsp-bridge-live-rename)
        (should called)))))

(ert-deftest lsp-bridge-live-rename-test-rename-still-asks-the-server ()
  "With a server, rename sends prepare_rename and then rename."
  (with-temp-buffer
    (insert "foo bar")
    (goto-char (point-min))
    (let ((lsp-bridge-mode t)
          calls)
      (cl-letf (((symbol-function 'lsp-bridge-has-lsp-server-p) (lambda () '("rust-analyzer")))
                ((symbol-function 'lsp-bridge--position) (lambda () (list :line 0 :character 0)))
                ((symbol-function 'read-string) (lambda (&rest _) "renamed"))
                ((symbol-function 'lsp-bridge-call-file-api)
                 (lambda (&rest args) (push args calls))))
        (call-interactively #'lsp-bridge-rename)
        (should (equal (list '("rename" (:line 0 :character 0) "renamed")
                             '("prepare_rename" (:line 0 :character 0)))
                       calls))))))


(ert-deftest lsp-bridge-live-rename-test-fallback-can-be-turned-off ()
  "With the fallback off both commands say there is no server."
  (with-temp-buffer
    (insert "foo bar foo")
    (goto-char (point-min))
    (let ((lsp-bridge-mode t)
          (lsp-bridge-rename-fallback-without-server nil)
          called)
      (cl-letf (((symbol-function 'lsp-bridge-has-lsp-server-p) (lambda () nil))
                ((symbol-function 'symbol-overlay-rename)
                 (lambda () (interactive) (setq called t))))
        (should-error (call-interactively #'lsp-bridge-rename) :type 'user-error)
        (should-error (call-interactively #'lsp-bridge-live-rename) :type 'user-error)
        (should-not called)))))


(provide 'lsp-bridge-live-rename-test)
;;; lsp-bridge-live-rename-test.el ends here
