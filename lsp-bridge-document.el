;;; lsp-bridge-document.el --- Documentation and signature help -*- lexical-binding: t -*-

;;; Commentary:

;; Part of lsp-bridge, loaded by lsp-bridge.el at the end of its own load.
;; Symbols that live in the other files of the package are declared below rather
;; than required, so that the files never need each other; a macro is autoloaded
;; with the `macro' flag, because a macro that the compiler does not know is
;; compiled as a function call.  Only libraries outside the package are
;; required.  The byte-compile gate reports a symbol that is used and not
;; declared, which keeps these lists honest.

;;; Code:

;; The frame helpers this file calls - `acm-frame-new', `acm-frame-hide-frame' -
;; and `acm-popup' are macros of the acm frontend, which requires nothing of
;; lsp-bridge, so requiring it here cannot make a cycle.
(require 'acm-frame)
(eval-when-compile (require 'acm-frame))

(defvar acm-frame--emacs-frame)
(declare-function acm-markdown-render-content "acm")
(declare-function lsp-bridge--point-position "lsp-bridge")
(declare-function lsp-bridge--position "lsp-bridge")
(declare-function lsp-bridge--position-in-org "lsp-bridge")
(defvar lsp-bridge-buffer-documentation-buffer)
(declare-function lsp-bridge-call-file-api "lsp-bridge")
(defvar lsp-bridge-cursor-after-command)
(defvar lsp-bridge-cursor-before-command)
(defvar lsp-bridge-enable-org-babel)
(declare-function lsp-bridge-has-lsp-server-p "lsp-bridge")
(defvar lsp-bridge-popup-documentation-buffer)

(defun lsp-bridge-show-documentation ()
  "Show the hover documentation of the symbol at point in its own buffer.
The active region, or point when there is none, selects the range
sent with the `hover' request."
  (interactive)
  (let* ((start (lsp-bridge--point-position (if (region-active-p) (region-beginning) (point))))
         (end (lsp-bridge--point-position (if (region-active-p) (region-end) (point)))))
    (lsp-bridge-call-file-api "hover" start end "show")))

(defun lsp-bridge-popup-documentation ()
  "Popup the hover documentation of the symbol at point.
The active region, or point when there is none, selects the range
sent with the `hover' request."
  (interactive)
  (let* ((start (lsp-bridge--point-position (if (region-active-p) (region-beginning) (point))))
         (end (lsp-bridge--point-position (if (region-active-p) (region-end) (point)))))
    (lsp-bridge-call-file-api "hover" start end "popup")))

(defun lsp-bridge-signature-help-fetch ()
  "Fetch the signature help of the call around point.
The request is skipped when no language server applies and when the
cursor did not move since the previous command; org buffers send the
position of the org src block instead of the buffer position."
  (interactive)
  (when (lsp-bridge-has-lsp-server-p)
    (unless (equal lsp-bridge-cursor-before-command lsp-bridge-cursor-after-command)
      (lsp-bridge-call-file-api "signature_help"
                                (if (and lsp-bridge-enable-org-babel (eq major-mode 'org-mode))
                                    (lsp-bridge--position-in-org)
                                  (lsp-bridge--position))))))

(defun lsp-bridge-switch-to-documentation-buffer (norecord)
  "Show the documentation buffer in another window.
NORECORD is passed to `switch-to-buffer-other-window'."
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create lsp-bridge-buffer-documentation-buffer) norecord))

(defun lsp-bridge-replace-html-entities ()
  "Replace `&nbsp;' entities with spaces in the current buffer.
The whole buffer is searched from the beginning."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "&nbsp;" nil t)
      (replace-match " "))))

(defun lsp-bridge-show-documentation--callback (value)
  "Render the hover documentation VALUE in the documentation buffer.
The buffer named by `lsp-bridge-buffer-documentation-buffer' is
reused, filled with the markdown rendered VALUE and displayed."
  (let ((buffer (get-buffer-create lsp-bridge-buffer-documentation-buffer)))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert value)
      (lsp-bridge-replace-html-entities)
      (setq-local truncate-lines nil)
      (acm-markdown-render-content t)
      (read-only-mode 1)
      (goto-char (point-min)))
    (display-buffer buffer 'display-buffer-reuse-window)))

(defun lsp-bridge-popup-documentation-scroll-up (&optional arg)
  "Scroll the hover documentation frame up by ARG lines.
ARG is passed to `scroll-up-command'; the call is ignored when the
documentation frame is not live."
  (interactive)
  (when (frame-live-p (acm-popup hover-frame))
    (with-selected-frame (acm-popup hover-frame)
      (apply #'scroll-up-command arg))))

(defun lsp-bridge-popup-documentation-scroll-down (&optional arg)
  "Scroll the hover documentation frame down by ARG lines.
ARG is passed to `scroll-down-command'; the call is ignored when the
documentation frame is not live."
  (interactive)
  (when (frame-live-p (acm-popup hover-frame))
    (with-selected-frame (acm-popup hover-frame)
      (apply #'scroll-down-command arg))))

(defun lsp-bridge-popup-documentation--callback (value)
  "Show the hover documentation VALUE in a popup acm frame.
The VALUE is rendered as markdown in
`lsp-bridge-popup-documentation-buffer' and the frame is created next
to the Emacs frame."
  (let ((emacs-frame (or acm-frame--emacs-frame (selected-frame))))
    (with-current-buffer (get-buffer-create lsp-bridge-popup-documentation-buffer)
      (read-only-mode -1)
      (erase-buffer)
      (insert value)
      (lsp-bridge-replace-html-entities)
      (setq-local truncate-lines nil)
      (acm-markdown-render-content))

    (acm-frame-new hover-frame
                   lsp-bridge-popup-documentation-buffer
                   "lsp bridge popup documentation frame"
                   (/ (frame-width emacs-frame) 2)
                   (/ (frame-height emacs-frame) 2))))

(defun lsp-bridge-hide-doc-tooltip ()
  "Hide the hover documentation popup frame."
  (acm-frame-hide-frame (acm-popup hover-frame)))

(defcustom lsp-bridge-signature-show-function 'message
  "Function used to render the signature help.
Set it to `lsp-bridge-signature-show-with-frame' to show the
signature in a popup frame instead of the echo area."
  :type 'function
  :group 'lsp-bridge)

(defcustom lsp-bridge-signature-show-with-frame-position "bottom-right"
  "Popup position of the signature frame.

Accepted values are `bottom-right' (the default), `top-left',
`top-right', `bottom-left' and `point'."
  :type 'string
  :group 'lsp-bridge)

(defcustom lsp-bridge-signature-buffer " *lsp-bridge-signature*"
  "Buffer for display signature information."
  :type 'string
  :group 'lsp-bridge)

(defun lsp-bridge-hide-signature-tooltip ()
  "Hide the signature help popup frame."
  (acm-frame-hide-frame (acm-popup signature-frame)))

(defun lsp-bridge-signature-show-with-frame (str)
  "Use popup frame to show the STR signatureHelp string."
  (if (not (string-empty-p str))
      (progn
        (with-current-buffer (get-buffer-create lsp-bridge-signature-buffer)
          (erase-buffer)
          (insert str)
          (visual-line-mode 1)
          (current-buffer))

        (acm-frame-new signature-frame
                       lsp-bridge-signature-buffer
                       "lsp bridge signature frame"
                       nil
                       nil
                       lsp-bridge-signature-show-with-frame-position))
    (lsp-bridge-hide-signature-tooltip)))

(defun lsp-bridge-signature-help--update (help-infos help-index)
  "Render the signature help built from HELP-INFOS.
HELP-INFOS is the list of signature strings and HELP-INDEX marks the
active one, which is highlighted; the result is shown through
`lsp-bridge-signature-show-function' and empty help is not shown."
  (let ((index 0)
        (help ""))
    (dolist (help-info help-infos)
      (setq help (concat help
                         (propertize help-info 'face (if (equal index help-index) 'font-lock-function-name-face 'default))
                         (if (equal index (1- (length help-infos))) "" ", ")))
      (setq index (1+ index)))

    (unless (string-equal help "")
      (let ((message-log-max nil))
        (funcall lsp-bridge-signature-show-function help)))))

(defvar lsp-bridge-signature-help-timer nil
  "Idle timer that triggers `lsp-bridge-signature-help-fetch'.")

(provide 'lsp-bridge-document)

;;; lsp-bridge-document.el ends here
