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
(declare-function symbol-overlay-rename "symbol-overlay")
(defvar lsp-bridge-mode)
(defvar lsp-bridge-mark-ring)
(defvar lsp-bridge-peek-ace-list)
(defvar lsp-bridge-position-before-jump)

;; Declared so that the bindings in `lsp-bridge-flash-region' are seen as the
;; dynamic ones they are: the pulse library reads these itself.
(defvar pulse-delay)
(defvar pulse-iterations)
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

(defcustom lsp-bridge-rename-fallback-without-server t
  "Rename the buffer occurrences when no server can be asked.
With a language server the rename always goes through it.  Without one, in an
Emacs Lisp buffer for instance, the request has nothing to answer it, and this
hands over to `symbol-overlay-rename' instead, which renames the occurrences
the buffer holds, inside a scope when it is set up for scopes.  Set it to nil
to keep both rename commands server only, saying that they cannot do anything
here."
  :type 'boolean
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
`read-string' and sends a `rename' request for the current position.

With no language server to ask there is nothing to send, and the request would
fail silently - which is what a buffer like an Emacs Lisp one looks like.  The
occurrences the buffer itself holds are renamed through
`symbol-overlay-rename' in that case, which also knows how to stay inside a
scope."
  (interactive)
  (cond
   ((and lsp-bridge-mode (lsp-bridge-has-lsp-server-p))
    (lsp-bridge-call-file-api "prepare_rename" (lsp-bridge--position))
    (let ((new-name (substring-no-properties (read-string "Rename to: " (thing-at-point 'symbol 'no-properties)))))
      (lsp-bridge-call-file-api "rename" (lsp-bridge--position) new-name)))
   ((and lsp-bridge-rename-fallback-without-server
         (fboundp 'symbol-overlay-rename))
    (call-interactively #'symbol-overlay-rename))
   (t
    (user-error "No language server here to rename through"))))

(defun lsp-bridge-flash-region (start-pos end-pos)
  "Flash the region from START-POS to END-POS.
The region is highlighted with `lsp-bridge-font-lock-flash' for
`lsp-bridge-flash-region-delay' seconds."
  (require 'pulse)
  ;; `pulse-delay' and `pulse-iterations' are read by the pulse library rather than
  ;; passed to it, so they are bound around the call: the compiler cannot see that
  ;; they are used, and dropping them as unused is what took the duration away once.
  (let ((pulse-delay lsp-bridge-flash-region-delay)
        (pulse-iterations 1))
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

;;; Rename

;;; Live rename

(declare-function lsp-bridge-document-highlight-render "lsp-bridge-workspace")
(declare-function lsp-bridge-document-highlight--position-to-point "lsp-bridge-workspace")
(defvar lsp-bridge-document-highlight-face)
(defvar lsp-bridge-document-highlight-hook)

(defface lsp-bridge-live-rename-face
  '((t :inherit lsp-bridge-document-highlight-face :underline t))
  "Face used for the occurrences of a live rename."
  :group 'lsp-bridge)

(defvar-local lsp-bridge-live-rename--overlays nil
  "Overlays of the live rename in this buffer, one per occurrence.")

(defvar-local lsp-bridge-live-rename--active nil
  "Cons of the markers around the occurrence the new name is typed in.")

(defvar-local lsp-bridge-live-rename--original nil
  "The text the symbol had when the live rename started.")

(defvar-local lsp-bridge-live-rename--name nil
  "The new name as it stands in the buffer right now.")

(defvar-local lsp-bridge-live-rename--pending nil
  "Non-nil while the highlight request that starts a live rename is out.")

(defvar-local lsp-bridge-live-rename--textual nil
  "Non-nil when the occurrences came from the buffer text, not from a server.")

(defvar lsp-bridge-live-rename-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'lsp-bridge-live-rename-commit)
    (define-key map (kbd "C-c C-k") #'lsp-bridge-live-rename-abort)
    map)
  "Keymap of `lsp-bridge-live-rename-mode'.")

(define-minor-mode lsp-bridge-live-rename-mode
  "Type the new name of a symbol and watch every occurrence follow.
The occurrences come from `textDocument/documentHighlight', the one at point is
edited in place and the others are displayed with the new name while the buffer
itself keeps its text - which is what keeps the positions, and the undo list,
exactly as the server knows them.  `lsp-bridge-live-rename-commit' then asks the
server for the real rename, so the other files are renamed as well."
  :lighter " Rename"
  :keymap lsp-bridge-live-rename-mode-map
  (if lsp-bridge-live-rename-mode
      (add-hook 'after-change-functions #'lsp-bridge-live-rename--after-change nil t)
    (remove-hook 'after-change-functions #'lsp-bridge-live-rename--after-change t)))

(defun lsp-bridge-live-rename--after-change (&rest _)
  "Show the name being typed on every occurrence but the one being typed."
  (when (and lsp-bridge-live-rename-mode
             (car lsp-bridge-live-rename--active)
             (cdr lsp-bridge-live-rename--active))
    (let ((name (buffer-substring-no-properties (car lsp-bridge-live-rename--active)
                                                (cdr lsp-bridge-live-rename--active))))
      (setq lsp-bridge-live-rename--name name)
      (dolist (overlay lsp-bridge-live-rename--overlays)
        (when (overlay-get overlay 'lsp-bridge-live-rename)
          (overlay-put overlay 'display name))))))

(defun lsp-bridge-live-rename--cleanup ()
  "Undo what the live rename put in the buffer, overlays and markers alike."
  (dolist (overlay lsp-bridge-live-rename--overlays)
    (delete-overlay overlay))
  (setq lsp-bridge-live-rename--overlays nil)
  (dolist (marker (list (car lsp-bridge-live-rename--active)
                        (cdr lsp-bridge-live-rename--active)))
    (when (markerp marker)
      (set-marker marker nil)))
  (setq lsp-bridge-live-rename--active nil)
  (setq lsp-bridge-live-rename--name nil)
  (setq lsp-bridge-live-rename--original nil)
  (setq lsp-bridge-live-rename--textual nil))

(defun lsp-bridge-live-rename--finish (commit)
  "Leave the live rename, applying it through the server when COMMIT is non-nil.
The occurrence that was edited in place is put back to the text the server last
saw before the rename is asked for, because the edit that comes back replaces
those very positions itself."
  (let ((name lsp-bridge-live-rename--name)
        (original lsp-bridge-live-rename--original)
        (textual lsp-bridge-live-rename--textual)
        (start (car lsp-bridge-live-rename--active))
        (end (cdr lsp-bridge-live-rename--active)))
    (lsp-bridge-live-rename-mode -1)
    (when (and (not textual) (markerp start) (marker-position start) original)
      (save-excursion
        (goto-char start)
        (delete-region start end)
        (insert original)
        (when (and commit name (not (string-empty-p name)) (not (equal name original)))
          ;; the rename is asked from the symbol itself, which is in place again
          (lsp-bridge-call-file-api "rename" (lsp-bridge--position) name))))
    (when (and commit textual name (not (string-empty-p name)) (not (equal name original)))
      ;; no server to apply anything: the occurrences are text, so they are
      ;; written here, and all of it inside this command, which is one undo step
      (let ((renamed 0))
        (save-excursion
          (dolist (overlay lsp-bridge-live-rename--overlays)
            (when (overlay-get overlay 'lsp-bridge-live-rename)
              (let ((overlay-start (overlay-start overlay))
                    (overlay-end (overlay-end overlay)))
                (when (and overlay-start overlay-end (< overlay-start overlay-end))
                  (goto-char overlay-start)
                  (delete-region overlay-start overlay-end)
                  (insert name)
                  (setq renamed (1+ renamed)))))))
        (message "Renamed %d %s." (1+ renamed)
                 (if (zerop renamed) "occurrence" "occurrences"))))
    (unless (and commit name (not (string-empty-p name)) (not (equal name original)))
      (message "Live rename: nothing changed"))
    (lsp-bridge-live-rename--cleanup)))

(defun lsp-bridge-live-rename-commit ()
  "Apply the live rename through the language server."
  (interactive)
  (lsp-bridge-live-rename--finish t))

(defun lsp-bridge-live-rename-abort ()
  "Give up the live rename and put the symbol back."
  (interactive)
  (lsp-bridge-live-rename--finish nil))

(defun lsp-bridge-live-rename--symbol-char-p (char)
  "Return non-nil when CHAR can be part of a symbol.
What counts is the syntax table of the buffer, so this holds for Emacs Lisp
symbols with dashes in them as much as for names in a language server's file."
  (memq (char-syntax (or char ?\s)) '(?w ?_)))

(defun lsp-bridge-live-rename--textual-ranges (symbol)
  "Return the ranges of the occurrences of SYMBOL in this buffer.
The characters around a match must not be symbol characters, which is what keeps
an occurrence inside a longer name out of the rename; case is significant, as it
is in the languages this is used for.  At most 1000 occurrences are collected,
so that a symbol in every line of a large file cannot make the walk of the
buffer unbounded."
  (let ((case-fold-search nil)
        (regexp (regexp-quote symbol))
        (limit 1000)
        ranges)
    (save-excursion
      (goto-char (point-min))
      (while (and (< (length ranges) limit) (re-search-forward regexp nil t))
        (let ((start (match-beginning 0))
              (end (match-end 0)))
          (when (and (not (lsp-bridge-live-rename--symbol-char-p (char-before start)))
                     (not (lsp-bridge-live-rename--symbol-char-p (char-after end))))
            (push (cons start end) ranges)))))
    (nreverse ranges)))

(defun lsp-bridge-live-rename--setup (ranges)
  "Start a live rename over RANGES, the occurrences inside this buffer.
The occurrence at point is emptied and typed into; the others get an overlay
that displays whatever is typed there."
  (let ((ranges (sort ranges (lambda (a b) (< (car a) (car b)))))
        (symbol (thing-at-point 'symbol t))
        active)
    (dolist (range ranges)
      (when (and (not active) (>= (point) (car range)) (<= (point) (cdr range)))
        (setq active range)))
    (cond
     ((null ranges) (message "Live rename: the server found no occurrence to rename"))
     ((or (null symbol) (string-empty-p symbol))
      (message "Live rename: no symbol at point"))
     (t
      (unless active (setq active (car ranges)))
      (setq lsp-bridge-live-rename--original symbol)
      (setq lsp-bridge-live-rename--name symbol)
      (setq lsp-bridge-live-rename--overlays
            (mapcar (lambda (range)
                      (let ((overlay (make-overlay (car range) (cdr range))))
                        (overlay-put overlay 'face 'lsp-bridge-live-rename-face)
                        (unless (equal range active)
                          ;; the buffer keeps its text: the overlay shows the name
                          (overlay-put overlay 'lsp-bridge-live-rename t)
                          (overlay-put overlay 'display symbol))
                        overlay))
                    ranges))
      ;; the markers grow around what is typed: the start stays before it and the
      ;; end follows it, so the text between them is the new name
      (setq lsp-bridge-live-rename--active
            (cons (copy-marker (car active))
                  (progn (goto-char (cdr active)) (copy-marker (cdr active) t))))
      (delete-region (car lsp-bridge-live-rename--active)
                     (cdr lsp-bridge-live-rename--active))
      (lsp-bridge-live-rename-mode 1)
      (message "Live rename: type the new name, C-c C-c applies it, C-c C-k gives up")))))

(defun lsp-bridge-live-rename--render-hook (highlights)
  "Start the live rename the highlights were asked for by HIGHLIGHTS."
  (when lsp-bridge-live-rename--pending
    (setq lsp-bridge-live-rename--pending nil)
    (let (ranges)
      (dolist (highlight highlights)
        (let* ((range (plist-get highlight :range))
               (start (and range (plist-get range :start)))
               (end (and range (plist-get range :end))))
          (when (and start end)
            (push (cons (lsp-bridge-document-highlight--position-to-point
                         (plist-get start :line) (plist-get start :character))
                        (lsp-bridge-document-highlight--position-to-point
                         (plist-get end :line) (plist-get end :character)))
                  ranges))))
      (lsp-bridge-live-rename--setup (nreverse ranges)))))

(defun lsp-bridge-live-rename ()
  "Rename the symbol at point while every occurrence follows along.
The occurrences are the ones the language server reports for the symbol, so what
is shown is what the server considers part of the symbol.  `C-c C-c' applies the
rename through the server, which is also what renames the occurrences in the
other files, and `C-c C-k' puts the buffer back."
  (interactive)
  (cond
   ((and lsp-bridge-mode (lsp-bridge-has-lsp-server-p))
    (when lsp-bridge-live-rename-mode
      (lsp-bridge-live-rename--finish nil))
    (setq lsp-bridge-live-rename--pending t)
    (lsp-bridge-call-file-api "document_highlight" (lsp-bridge--position)))
   ;; Without a server there are no highlights to ask for, and the occurrences
   ;; are what the buffer text holds: they are found here and renamed as text.
   (lsp-bridge-rename-fallback-without-server
    (let ((symbol (thing-at-point 'symbol t)))
      (if (or (null symbol) (string-empty-p symbol))
          (user-error "No symbol at point to rename")
        (setq lsp-bridge-live-rename--textual t)
        (lsp-bridge-live-rename--setup
         (lsp-bridge-live-rename--textual-ranges symbol)))))
   (t
    (user-error "No language server here to rename through"))))

(add-hook 'lsp-bridge-document-highlight-hook #'lsp-bridge-live-rename--render-hook)

;; The count is said out loud for the occurrences that were written here, since no
;; server edit comes back to report them.


(provide 'lsp-bridge-jump)

;;; lsp-bridge-jump.el ends here
