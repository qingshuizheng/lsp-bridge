;;; lsp-bridge-ui-test.el --- What the mode line is told  -*- lexical-binding: t; -*-

;;; Commentary:
;; The mode line of a buffer shows what the language server is doing, which the
;; server reports as a message like `Loading (96%)', and the name of the definition
;; point is in, which is kept in the buffer.  Parsing that message into the text the
;; mode line shows, and keeping and reading back the definition name, need neither
;; a server nor a frame, and are what is checked here.  The rest of the module reads
;; the state of the whole package for the mode line, which is not text.

;;; Code:

(require 'ert)
(require 'lsp-bridge)
(require 'lsp-bridge-ui)

(ert-deftest lsp-bridge-ui-test-progress-with-a-stage-and-a-percent ()
  "A stage and a percentage become the two of them, with no icon here.
The icon is left out on an Emacs without the icon library, which is why the text
starts with the space that would have separated it, and the percentage was dropped
before this fix because the pattern looked for `%%' where the message has one."
  (should (equal (lsp-bridge--format-progress-message "Loading (96%)")
                 " Loading 96%"))
  ;; The rustAnalyzer prefix is not part of the stage name.
  (should (equal (lsp-bridge--format-progress-message "rustAnalyzer/Indexing (12%)")
                 " Indexing 12%")))

(ert-deftest lsp-bridge-ui-test-progress-with-only-a-stage ()
  "A message without a percentage keeps the stage alone."
  (should (equal (lsp-bridge--format-progress-message "Indexing") " Indexing")))

(ert-deftest lsp-bridge-ui-test-progress-without-a-stage ()
  "A message the pattern does not match gives nothing to show."
  (should (equal (lsp-bridge--format-progress-message "(96%)") ""))
  (should (equal (lsp-bridge--format-progress-message "") "")))

(ert-deftest lsp-bridge-ui-test-current-defun-is-kept-per-buffer ()
  "The definition name is stored in the buffer it belongs to."
  (with-temp-buffer
    (should-not (lsp-bridge-symbols--current-defun))
    (lsp-bridge-symbols--record-current-defun "my-function")
    (should (equal (lsp-bridge-symbols--current-defun) "my-function")))
  (with-temp-buffer
    ;; A fresh buffer has none of its own, which is the point of it being local.
    (should-not (lsp-bridge-symbols--current-defun))))

(defun lsp-bridge-ui-test--mode-line (server live progress)
  "Return the mode line text with SERVER, LIVE and PROGRESS in place."
  (let ((lsp-bridge-server server)
        (lsp-bridge--work-done-progress progress))
    (cl-letf (((symbol-function 'lsp-bridge-process-live-p) (lambda () live)))
      (lsp-bridge--mode-line-format))))

(ert-deftest lsp-bridge-ui-test-mode-line-without-a-server ()
  "There is nothing to show before a server is there."
  (should-not (lsp-bridge-ui-test--mode-line nil t nil)))

(ert-deftest lsp-bridge-ui-test-mode-line-says-whether-the-server-lives ()
  "The face of the name is the state of the server, and the progress follows it."
  (let ((alive (lsp-bridge-ui-test--mode-line "server" t nil))
        (dead (lsp-bridge-ui-test--mode-line "server" nil nil))
        (busy (lsp-bridge-ui-test--mode-line "server" t " Indexing 12%")))
    (should (equal (substring-no-properties alive) "lsp-bridge"))
    (should (eq (get-text-property 0 'face alive) 'lsp-bridge-alive-mode-line))
    (should (eq (get-text-property 0 'face dead) 'lsp-bridge-kill-mode-line))
    ;; The progress carries the space an icon would have filled, so without the
    ;; icon library the mode line has two of them.
    (should (equal (substring-no-properties busy) "lsp-bridge  Indexing 12%"))
    (should (eq (get-text-property (length "lsp-bridge ") 'face busy)
                'lsp-bridge-alive-mode-line))))

(ert-deftest lsp-bridge-ui-test-record-progress-keeps-it-per-buffer ()
  "The progress is stored in the buffer of the file it is about."
  (let ((file (make-temp-file "lsp-bridge-ui-test" nil ".py"))
        (acm-backend-lsp-show-progress nil))
    (unwind-protect
        (with-current-buffer (find-file-noselect file)
          (setq-local lsp-bridge--work-done-progress-timer nil)
          (lsp-bridge--record-work-done-progress "Indexing (12%)" (list file))
          (should (equal lsp-bridge--work-done-progress " Indexing 12%"))
          (should (timerp lsp-bridge--work-done-progress-timer))
          (lsp-bridge--record-work-done-progress "" (list file))
          (should-not lsp-bridge--work-done-progress)
          (when lsp-bridge--work-done-progress-timer
            (cancel-timer lsp-bridge--work-done-progress-timer)
            (setq-local lsp-bridge--work-done-progress-timer nil)))
      (let ((buffer (get-file-buffer file)))
        (when buffer (kill-buffer buffer)))
      (delete-file file))))

(provide 'lsp-bridge-ui-test)
;;; lsp-bridge-ui-test.el ends here
