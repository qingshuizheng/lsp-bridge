;;; lsp-bridge-jump.el --- Jump, find and rename commands -*- lexical-binding: t -*-

;;; Commentary:

;; Part of lsp-bridge, loaded by lsp-bridge.el at the end of its own load.
;; Symbols that live in the other files of the package are declared below rather
;; than required, so that the files never need each other; a macro is autoloaded
;; with the `macro' flag, because a macro that the compiler does not know is
;; compiled as a function call.  Only libraries outside the package are
;; required.  The byte-compile gate reports a symbol that is used and not
;; declared, which keeps these lists honest.

;;; Code:

(require 'tramp)

(declare-function acm-backend-elisp-find-def "acm-backend-elisp")
(declare-function acm-backend-lsp-position-to-point "acm-backend-lsp")
(declare-function acm-is-elisp-mode-p "acm")
(declare-function lsp-bridge--position "lsp-bridge")
(declare-function lsp-bridge--resolve-remote-host "lsp-bridge")
(require 'lsp-bridge-core)

;; The macros of `lsp-bridge-core' expand into calls to these, which this
;; file does not define.
(declare-function lsp-bridge--resolve-remote-host "lsp-bridge")
(declare-function lsp-bridge-get-match-buffer-by-filepath "lsp-bridge")
(declare-function lsp-bridge-get-match-buffer-by-remote-file "lsp-bridge")
(declare-function lsp-bridge-call-async "lsp-bridge")
(declare-function lsp-bridge-call-file-api "lsp-bridge")
(defvar lsp-bridge-enable-with-tramp)
(defvar lsp-bridge-flash-region-delay)
(defvar lsp-bridge-font-lock-flash)
(declare-function lsp-bridge-has-lsp-server-p "lsp-bridge")
(defvar lsp-bridge-mark-ring)
(defvar lsp-bridge-peek-ace-list)
(defvar lsp-bridge-position-before-jump)
(declare-function lsp-bridge-ref-popup "lsp-bridge-ref")
(defvar lsp-bridge-tramp-alias-alist)

(defcustom lsp-bridge-find-def-fallback-function nil
  "Fallback for find definition failure."
  :type 'function
  :group 'lsp-bridge)

(defcustom lsp-bridge-find-ref-fallback-function nil
  "Fallback for find referecences failure."
  :type 'function
  :group 'lsp-bridge)

(defcustom lsp-bridge-find-def-select-in-open-windows nil
  "Whether to open definitions in already open windows.
When non-nil, an already open window showing the definition is
selected instead of switching buffers; off by default."
  :type 'boolean
  :group 'lsp-bridge)

(defvar-local lsp-bridge-jump-to-def-in-other-window nil
  "Whether the next definition jump should use another window.")

(defun lsp-bridge-find-def ()
  "Jump to the definition of the symbol at point in the current window.
Elisp buffers use the elisp backend, other buffers ask the language
server or `lsp-bridge-find-def-fallback-function' when no server is
available."
  (interactive)
  (cond
   ((acm-is-elisp-mode-p)
    (lsp-bridge--record-mark-ring)
    (acm-backend-elisp-find-def)
    (lsp-bridge--set-mark-ring-in-new-buffer))
   (t
    (setq-local lsp-bridge-jump-to-def-in-other-window nil)
    (if (lsp-bridge-has-lsp-server-p)
        (lsp-bridge-call-file-api "find_define" (lsp-bridge--position))
      (lsp-bridge-find-def-fallback (lsp-bridge--position))))))

(defun lsp-bridge-find-def-other-window ()
  "Jump to the definition of the symbol at point in another window.
Like `lsp-bridge-find-def' but sets
`lsp-bridge-jump-to-def-in-other-window' for the jump."
  (interactive)
  (cond
   ((acm-is-elisp-mode-p)
    (lsp-bridge--record-mark-ring)
    (acm-backend-elisp-find-def)
    (lsp-bridge--set-mark-ring-in-new-buffer))
   (t
    (setq-local lsp-bridge-jump-to-def-in-other-window t)
    (if (lsp-bridge-has-lsp-server-p)
        (lsp-bridge-call-file-api "find_define" (lsp-bridge--position))
      (lsp-bridge-find-def-fallback (lsp-bridge--position))))))

(defun lsp-bridge-find-def-return ()
  "Pop off lsp-bridge-mark-ring and jump to the top location."
  (interactive)
  ;; Pop entries that refer to non-existent buffers.
  (while (and lsp-bridge-mark-ring (not (marker-buffer (car lsp-bridge-mark-ring))))
    (setq-local lsp-bridge-mark-ring (cdr lsp-bridge-mark-ring)))
  (or lsp-bridge-mark-ring
      (error "[LSP-Bridge] No lsp-bridge mark set"))
  (let* ((this-buffer (current-buffer))
         (marker (pop lsp-bridge-mark-ring))
         (buffer (marker-buffer marker))
         (position (marker-position marker)))
    (set-buffer buffer)
    (or (and (>= position (point-min))
             (<= position (point-max)))
        (if widen-automatically
            (widen)
          (error "[LSP-Bridge] mark position is outside accessible part of buffer %s"
                 (buffer-name buffer))))
    (goto-char position)
    (unless (equal buffer this-buffer)
      (switch-to-buffer buffer))
    (recenter)))

(defun lsp-bridge-find-type-def ()
  "Jump to the type definition of the symbol at point.
Sends a `find_type_define' request for the current position in the
current window."
  (interactive)
  (setq-local lsp-bridge-jump-to-def-in-other-window nil)
  (lsp-bridge-call-file-api "find_type_define" (lsp-bridge--position)))

(defun lsp-bridge-find-type-def-other-window ()
  "Jump to the type definition of the symbol at point in another window.
Like `lsp-bridge-find-type-def' but sets
`lsp-bridge-jump-to-def-in-other-window' for the jump."
  (interactive)
  (setq-local lsp-bridge-jump-to-def-in-other-window t)
  (lsp-bridge-call-file-api "find_type_define" (lsp-bridge--position)))

(defun lsp-bridge-find-impl ()
  "Jump to the implementation of the symbol at point.
Sends a `find_implementation' request for the current position in the
current window."
  (interactive)
  (setq-local lsp-bridge-jump-to-def-in-other-window nil)
  (lsp-bridge-call-file-api "find_implementation" (lsp-bridge--position)))

(defun lsp-bridge-find-impl-other-window ()
  "Jump to the implementation of the symbol at point in another window.
Like `lsp-bridge-find-impl' but sets
`lsp-bridge-jump-to-def-in-other-window' for the jump."
  (interactive)
  (setq-local lsp-bridge-jump-to-def-in-other-window t)
  (lsp-bridge-call-file-api "find_implementation" (lsp-bridge--position)))

(defun lsp-bridge-find-references ()
  "List the references of the symbol at point.
Sends a `find_references' request when a language server applies,
otherwise calls `lsp-bridge-find-ref-fallback-function'."
  (interactive)
  (if (lsp-bridge-has-lsp-server-p)
      (lsp-bridge-call-file-api "find_references" (lsp-bridge--position))
    (lsp-bridge-find-ref-fallback (lsp-bridge--position))))

(defun lsp-bridge-find-def-fallback (position)
  "Report that no definition was found at POSITION.
When a peek buffer is open it is closed and its target buffer becomes
current; `lsp-bridge-find-def-fallback-function' is called with
POSITION when it is set."
  (if (not (= (length lsp-bridge-peek-ace-list) 0))
      (progn
	    (if (nth 0 lsp-bridge-peek-ace-list)
	        (kill-buffer (nth 0 lsp-bridge-peek-ace-list)))
	    (switch-to-buffer (nth 2 lsp-bridge-peek-ace-list))
	    (goto-char (nth 1 lsp-bridge-peek-ace-list))))
  (message "[LSP-Bridge] No definition found.")
  (if (functionp lsp-bridge-find-def-fallback-function)
      (funcall lsp-bridge-find-def-fallback-function position)))

(defun lsp-bridge-find-ref-fallback (position)
  "Report that no reference was found at POSITION.
Calls `lsp-bridge-find-ref-fallback-function' with POSITION when it
is set."
  (message "[LSP-Bridge] No references found.")
  (if (functionp lsp-bridge-find-ref-fallback-function)
      (funcall lsp-bridge-find-ref-fallback-function position)))

(defun lsp-bridge-references--popup (references-content references-counter position)
  "Show the references collected for POSITION.
REFERENCES-CONTENT holds the reference entries and
REFERENCES-COUNTER their number; the lsp-bridge ref buffer is opened
when there is at least one reference, otherwise
`lsp-bridge-find-ref-fallback' is called for POSITION."
  (if (> references-counter 0)
      (progn
        (lsp-bridge-ref-popup references-content references-counter)
        (message "[LSP-Bridge] Found %s references" references-counter))
    (lsp-bridge-find-ref-fallback position)))

(defun lsp-bridge-rename ()
  "Rename the symbol at point.
Sends a `prepare_rename' request, asks for the new name with
`read-string' and sends a `rename' request for the current position."
  (interactive)
  (lsp-bridge-call-file-api "prepare_rename" (lsp-bridge--position))
  (let ((new-name (substring-no-properties (read-string "Rename to: " (thing-at-point 'symbol 'no-properties)))))
    (lsp-bridge-call-file-api "rename" (lsp-bridge--position) new-name)))

(defun lsp-bridge-flash-region (start-pos end-pos)
  "Flash the region from START-POS to END-POS.
The region is highlighted with `lsp-bridge-font-lock-flash' for
`lsp-bridge-flash-region-delay' seconds."
  (require 'pulse)
  (let ((pulse-iterations 1)
        (pulse-delay lsp-bridge-flash-region-delay))
    (pulse-momentary-highlight-region start-pos end-pos 'lsp-bridge-font-lock-flash)))

(defun lsp-bridge-flash-line ()
  "Flash the line around point.
The whole current line is passed to `lsp-bridge-flash-region'."
  (lsp-bridge-flash-region
   (save-excursion
     (vertical-motion 0) (point))
   (save-excursion
     (vertical-motion 1) (point))))

(defun lsp-bridge-rename--highlight (filename filehost bound-start bound-end)
  "Flash the renamed region of FILENAME on FILEHOST.
BOUND-START and BOUND-END are LSP positions and are converted to
buffer positions before `lsp-bridge-flash-region' is called."
  (lsp-bridge--with-file-buffer filename filehost
                                (lsp-bridge-flash-region
                                 (acm-backend-lsp-position-to-point bound-start)
                                 (acm-backend-lsp-position-to-point bound-end))))

(defun lsp-bridge--record-mark-ring ()
  "Record the position to come back to before a jump.
Call it in the old buffer before jumping and
`lsp-bridge--set-mark-ring-in-new-buffer' in the new buffer
afterwards; the two functions must be used in pairs."
  ;; Record postion.
  (let ((marker (set-marker (mark-marker) (point) (current-buffer))))
    (setq lsp-bridge-position-before-jump (copy-marker marker)))
  (setq mark-ring lsp-bridge-mark-ring))

(defun lsp-bridge--set-mark-ring-in-new-buffer ()
  "Push the recorded jump position onto `lsp-bridge-mark-ring'.
Must be called in the new buffer after `lsp-bridge--record-mark-ring'
was called in the old one."
  (setq-local lsp-bridge-mark-ring (append (list lsp-bridge-position-before-jump) mark-ring)))

(defun lsp-bridge-define--jump (filename filehost position)
  "Jump to POSITION in FILENAME on FILEHOST.
The current position is recorded on the mark ring first.  Remote hosts
are resolved and the file is opened in an existing window when one
shows it, otherwise it is visited in the current or another window;
remote files are opened asynchronously through the server."
  (let (lsp-bridge-position-before-jump)
    (lsp-bridge--record-mark-ring)

    (if (and (not (string= filehost ""))
             (not lsp-bridge-enable-with-tramp))
        (lsp-bridge-call-async "open_remote_file" (format "%s:%s" filehost filename) position)
      ;; filehost is not empty or lsp-bridge-enable-with-tramp is t
      ;; Resolve IP-to-hostname for ProxyCommand hosts where the remote
      ;; server reports client_address IP instead of the SSH hostname.
      (setq filehost (lsp-bridge--resolve-remote-host filehost))

      (let ((match-window (lsp-bridge--with-file-buffer filename filehost (get-buffer-window))))
        ;; select the window to display definition
        (if match-window
            ;; if match-window is found, avoid using find-file to open the file twice
            (progn
              (cond
               (lsp-bridge-find-def-select-in-open-windows (select-window match-window))
               (lsp-bridge-jump-to-def-in-other-window (select-window match-window))
               (t (switch-to-buffer (window-buffer match-window)))))
          ;; match-window not found, we need to open the file
          (let* ((tramp-file-name (concat (cdr (assoc filehost lsp-bridge-tramp-alias-alist)) filename)))
            (if lsp-bridge-jump-to-def-in-other-window
                (find-file-other-window tramp-file-name)
              (find-file tramp-file-name))))

        ;; Init jump history in new buffer.
        (lsp-bridge--set-mark-ring-in-new-buffer)

        (lsp-bridge-define--jump-flash position)))))

(defun lsp-bridge-define--jump-flash (position)
  "Jump to POSITION and flash the target line.
POSITION is an LSP position; the window is recentered around the
target and `lsp-bridge-flash-line' highlights it."
  ;; We need call `display' before `goto-char',
  ;; otherwise Emacs won't jump to target line if file has opened.
  (redisplay)

  ;; Jump to define postion.
  (goto-char (acm-backend-lsp-position-to-point position))
  (recenter)

  ;; Flash define line.
  (lsp-bridge-flash-line))

(provide 'lsp-bridge-jump)

;;; lsp-bridge-jump.el ends here
