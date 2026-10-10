;;; lsp-bridge-ui.el --- Emacs-side UI glue of lsp-bridge -*- lexical-binding: t -*-

;;; Commentary:

;; What lsp-bridge shows in the parts of Emacs that are not the completion menu:
;; the function around point for `which-func', the progress of long running
;; requests, and the state of the server in the mode line.  The completion menu
;; itself is the acm/ frontend, which talks to the same server through the
;; functions of lsp-bridge.el.
;;
;; lsp-bridge.el requires this file at the end of its own load, so the symbols
;; it uses from there - the server process, the minor mode, the options - are
;; declared below instead of required: that is what keeps the two files from
;; needing each other, and it is why the declarations have to be kept in sync
;; with what the code actually uses (the byte-compile gate reports a symbol that
;; is used and not declared).

;;; Code:

(declare-function lsp-bridge-get-match-buffer-by-filepath "lsp-bridge" (name))
(declare-function lsp-bridge-process-live-p "lsp-bridge" ())
(defvar acm-backend-lsp-show-progress)
(defvar lsp-bridge--work-done-progress)
(defvar lsp-bridge--work-done-progress-timer)
(defvar lsp-bridge-enable-mode-line)
(defvar lsp-bridge-mode)
(defvar lsp-bridge-server)

;;; support which-func-mode
;;;

(defvar-local lsp-bridge-symbols-current-defun nil
  "Symbol of the function around point, as reported by the server.")

(defun lsp-bridge-symbols--record-current-defun (current-defun)
  "Record `CURRENT-DEFUN' from lsp."
  (setq-local lsp-bridge-symbols-current-defun current-defun))

(defun lsp-bridge-symbols--current-defun ()
  "Add `lsp-bridge-symbols-current-defun' to `which-func-functions'."
  lsp-bridge-symbols-current-defun)


(defun lsp-bridge--record-work-done-progress (progress file-paths)
  "Show PROGRESS for the buffers visiting FILE-PATHS.
PROGRESS is echoed when `acm-backend-lsp-show-progress' is non-nil and
is stored in the buffer-local `lsp-bridge--work-done-progress' for
the mode line, with a timer that clears it after a while."
  ;; Optional message output, controlled by acm-backend-lsp-show-progress.
  (when acm-backend-lsp-show-progress
    (unless (active-minibuffer-window)
      (message progress)))
  ;; Update mode-line progress indicator.
  (dolist (filepath file-paths)
    (when-let* ((buf (lsp-bridge-get-match-buffer-by-filepath filepath)))
      (with-current-buffer buf
        (when lsp-bridge--work-done-progress-timer
          (cancel-timer lsp-bridge--work-done-progress-timer)
          (setq-local lsp-bridge--work-done-progress-timer nil))
        (setq-local lsp-bridge--work-done-progress
                    (if (string-equal progress "")
                        nil
                      (lsp-bridge--format-progress-message progress)))
        (unless (string-equal progress "")
          (let ((buf buf))
            (setq-local lsp-bridge--work-done-progress-timer
                        (run-with-timer 1 nil
                                        (lambda ()
                                          (when (buffer-live-p buf)
                                            (with-current-buffer buf
                                              (setq lsp-bridge--work-done-progress nil)
                                              (force-mode-line-update t))))))))
        (force-mode-line-update)))))

(defun lsp-bridge--format-progress-message (progress)
  "Return the mode line text for the work done PROGRESS message."
  (let* (;; Extract percentage like (96%%)
         (percent-match (string-match "\\(([0-9]+%%)\\)" progress))
         (percent (if percent-match
                      (replace-regexp-in-string "[()]" "" (match-string 1 progress))
                    ""))
         ;; Extract stage name: strip rustAnalyzer/ prefix, take word before space/paren
         (stage (when (string-match "\\(?:rustAnalyzer/\\)?\\([A-Za-z ]+?\\)\\(?:[[:space:]]*(\\|[[:space:]]+[0-9]\\|$\\)" progress)
                  (string-trim (match-string 1 progress))))
         (icon (when (fboundp 'nerd-icons-codicon)
                 (nerd-icons-codicon "nf-cod-sync"))))
    (cond
     ;; Both stage and percent
     ((and stage (not (string-blank-p percent)))
      (format "%s %s %s" (or icon "") stage percent))
     ;; Only stage
     (stage
      (format "%s %s" (or icon "") stage))
     ;; Fallback
     (t ""))))

;;; Mode-line
;;;

(defface lsp-bridge-alive-mode-line
  '((t (:inherit font-lock-constant-face :weight bold)))
  "Face for activity in LSP-bridge's mode line."
  :group 'lsp-bridge)

(defface lsp-bridge-kill-mode-line
  '((t (:inherit font-lock-comment-face :weight bold)))
  "Face for kill process in LSP-bridge's mode line."
  :group 'lsp-bridge)

(defvar lsp-bridge--mode-line-format `(:eval (lsp-bridge--mode-line-format))
  "Mode line construct that shows the lsp-bridge server state.")

(put 'lsp-bridge--mode-line-format 'risky-local-variable t)

(defun lsp-bridge--mode-line-format ()
  "Compose the LSP-bridge's mode-line.
The face says whether the server is alive; the progress text is appended
while a long running request reports one."
  (when lsp-bridge-server
    (let ((mode-face (if (lsp-bridge-process-live-p)
                         'lsp-bridge-alive-mode-line
                       'lsp-bridge-kill-mode-line)))
      (concat (propertize "lsp-bridge" 'face mode-face)
              (when lsp-bridge--work-done-progress
                (propertize (concat " " lsp-bridge--work-done-progress)
                            'face 'lsp-bridge-alive-mode-line))))))

(when lsp-bridge-enable-mode-line
  (add-to-list 'mode-line-misc-info
               `(lsp-bridge-mode ("" lsp-bridge--mode-line-format " "))))

(provide 'lsp-bridge-ui)

;;; lsp-bridge-ui.el ends here
