;;; lsp-bridge-core.el --- Shared buffer state, change monitors and the minor mode -*- lexical-binding: t -*-

;;; Commentary:

;; Part of lsp-bridge, loaded by lsp-bridge.el at the end of its own load.
;; Symbols that live in the other files of the package are declared below rather
;; than required, so that the files never need each other; a macro is autoloaded
;; with the `macro' flag, because a macro that the compiler does not know is
;; compiled as a function call.  Only libraries outside the package are
;; required.  The byte-compile gate reports a symbol that is used and not
;; declared, which keeps these lists honest.

;;; Code:

(require 'cl-lib)

;;; The macros every other file of the package uses
;;
;; They live here, in the one file that requires nothing else of the
;; package, because a macro that a file cannot load without a cycle is
;; silently compiled as a function call (see the autoloads this used to
;; need).  A file that uses them requires this one.

(cl-defmacro lsp-bridge--with-file-buffer (filename filehost &rest body)
  "Evaluate BODY in the buffer visiting FILENAME on FILEHOST.
FILEHOST is resolved to a known host first and the buffer is looked up
by path for local hosts or by host and path for remote ones."
  (declare (indent 1))
  `(let ((resolved-host (lsp-bridge--resolve-remote-host ,filehost)))
     (when-let* ((buffer (pcase resolved-host
                           ("" (lsp-bridge-get-match-buffer-by-filepath ,filename))
                           (_ (lsp-bridge-get-match-buffer-by-remote-file resolved-host ,filename)))))
       (with-current-buffer buffer
         ,@body))))

(cl-defmacro lsp-bridge-save-position (&rest body)
  "Evaluate BODY and restore point afterwards.
`save-excursion' is not enough for LSP code format, so the line, the
column and the indentation column are recorded before BODY runs and
restored after it."
  `(let* ((current-buf (current-buffer))
          (current-line (line-number-at-pos nil t))
          (current-column (lsp-bridge--calculate-column))
          (indent-column (save-excursion
                           (back-to-indentation)
                           (lsp-bridge--calculate-column))))
     ,@body
     (switch-to-buffer current-buf)
     (goto-line current-line)
     (back-to-indentation)
     (forward-char (max (- current-column indent-column) 0))))

;; The macros above expand into calls to these, so every file that uses them
;; needs them declared as well.
(declare-function lsp-bridge--resolve-remote-host "lsp-bridge-remote")
(declare-function lsp-bridge-get-match-buffer-by-filepath "lsp-bridge")
(declare-function lsp-bridge-get-match-buffer-by-remote-file "lsp-bridge")

(defvar acm-backend-capf-mode-hooks)
(declare-function acm-backend-elisp-get-symbols "acm-backend-elisp")
(defvar acm-backend-elisp-search-max-number)
(defvar acm-backend-elisp-symbols-update-size)
(defvar acm-backend-elisp-symbols-update-timer)
(defvar acm-backend-lsp-fetch-completion-item-ticker)
(declare-function acm-backend-lsp-init "acm-backend-lsp")
(declare-function acm-backend-lsp-workspace-symbol-init "acm-backend-lsp-workspace-symbol")
(autoload 'acm-cancel-timer "acm" nil nil 'macro)
(declare-function acm-char-before "acm")
(declare-function acm-complete "acm")
(declare-function acm-filter-delete-char "acm")
(declare-function acm-filter-insert-char "acm")
(defvar acm-filter-overlay)
(declare-function acm-get-input-prefix "acm")
(declare-function acm-hide "acm")
(declare-function acm-in-comment-p "acm")
(declare-function acm-in-string-p "acm")
(declare-function acm-is-elisp-mode-p "acm")
(defvar acm-preview-overlay)
(autoload 'acm-run-idle-func "acm" nil nil 'macro)
(declare-function acm-session-popup-buffer-p "acm-session")
(declare-function lsp-bridge-auto-format-code "lsp-bridge-workspace")
(defvar lsp-bridge-auto-format-code-idle)
(declare-function lsp-bridge-buffer-file-name "lsp-bridge")
(declare-function lsp-bridge-call-async "lsp-bridge-transport")
(declare-function lsp-bridge-call-file-api "lsp-bridge-transport")
(declare-function lsp-bridge-call-file-api-p "lsp-bridge-transport")
(declare-function lsp-bridge-check-predicate "lsp-bridge-completion")
(declare-function lsp-bridge-code-action-popup-quit "lsp-bridge-code-action")
(declare-function lsp-bridge-complete-other-backends "lsp-bridge-completion")
(defvar lsp-bridge-default-mode-hooks)
(declare-function lsp-bridge-diagnostic-hide-overlays "lsp-bridge-diagnostic")
(declare-function lsp-bridge-diagnostic-hide-tooltip "lsp-bridge-diagnostic")
(declare-function lsp-bridge-diagnostic-maybe-display-error-at-point "lsp-bridge-diagnostic")
(defvar lsp-bridge-disable-backup)
(declare-function lsp-bridge-document-highlight-setup "lsp-bridge-workspace")
(defvar lsp-bridge-elisp-symbols-update-idle)
(defvar lsp-bridge-enable-auto-format-code)
(defvar lsp-bridge-enable-completion-in-minibuffer)
(defvar lsp-bridge-enable-document-highlight)
(declare-function lsp-bridge-enable-document-highlight-timer "lsp-bridge-workspace")
(defvar lsp-bridge-enable-hover-diagnostic)
(defvar lsp-bridge-enable-org-babel)
(defvar lsp-bridge-enable-predicates)
(defvar lsp-bridge-enable-search-words)
(defvar lsp-bridge-enable-signature-help)
(declare-function lsp-bridge-get-buffer-file-name-text "lsp-bridge")
(declare-function lsp-bridge-has-lsp-server-p "lsp-bridge")
(declare-function lsp-bridge-hide-doc-tooltip "lsp-bridge-document")
(declare-function lsp-bridge-hide-signature-tooltip "lsp-bridge-document")
(declare-function lsp-bridge-inlay-hint-hide-overlays "lsp-bridge-inlay-hint")
(declare-function lsp-bridge-inlay-hint-monitor-window-scroll "lsp-bridge-inlay-hint")
(declare-function lsp-bridge-inlay-hint-try-send-request "lsp-bridge-inlay-hint")
(declare-function lsp-bridge-is-remote-file "lsp-bridge")
(defvar lsp-bridge-last-change-command)
(defvar lsp-bridge-last-change-is-delete-command-p)
(declare-function lsp-bridge-org-babel-monitor-after-change "lsp-bridge-org-babel")
(declare-function lsp-bridge-org-babel-send-src-block-to-lsp-server "lsp-bridge-org-babel")
(declare-function lsp-bridge-process-live-p "lsp-bridge-transport")
(declare-function lsp-bridge-record-last-change-position "lsp-bridge-completion")
(defvar lsp-bridge-remote-file-host)
(defvar lsp-bridge-remote-file-path)
(declare-function lsp-bridge-remote-send-func-request "lsp-bridge-remote")
(defvar lsp-bridge-search-words-prohibit-file-extensions)
(declare-function lsp-bridge-signature-help-fetch "lsp-bridge-document")
(defvar lsp-bridge-signature-help-fetch-idle)
(defvar lsp-bridge-signature-help-timer)
(declare-function lsp-bridge-start-process "lsp-bridge-transport")
(declare-function lsp-bridge-symbols--current-defun "lsp-bridge-ui")
(defvar lsp-bridge-symbols-enable-which-func)
(declare-function lsp-bridge-try-completion "lsp-bridge-completion")

(defvar-local lsp-bridge-last-cursor-position 0
  "Cursor position seen by the last `post-command-hook' call.")

(defvar-local lsp-bridge-prohibit-completion nil
  "Whether completion is temporarily prohibited in this buffer.")

(defvar-local lsp-bridge-cursor-before-command 0
  "Cursor position recorded before the current command runs.")

(defvar-local lsp-bridge-cursor-after-command 0
  "Cursor position recorded after the current command ran.")

;;; Naming the command that is running
;;
;; Every monitor below has to compare the command that is running, and the
;; objects `this-command' and `last-command' hold are not always symbols.

(defun lsp-bridge-command-name (&optional command)
  "Return the name of COMMAND, or of `this-command', as a string.
A command is normally a symbol, and then this is just its name.  It can also
be something else, because a package is free to put anything in there: meow
stores the result of `key-binding', which is a closure when the key is bound
to a lambda, and `last-command' then carries that object on.  Formatting such
an object prints it - for a closure that means its whole environment - and
`prin1' builds that string in C without checking for quit, so a big one (an
application runtime, say) makes Emacs ignore \\[keyboard-quit] until it is killed.
Anything that is not a symbol is therefore named by its type."
  (let ((command (or command this-command)))
    (if (symbolp command)
        (symbol-name command)
      (format "%s" (type-of command)))))

(defun lsp-bridge-monitor-pre-command ()
  "Record the cursor before a command and prepare acm state.
Runs in `pre-command-hook': it saves the cursor position, completes a
pending acm preview and rewrites `this-command' while an acm filter
overlay accepts the typed characters."
  (setq-local lsp-bridge-cursor-before-command (point))

  ;; Tab-and-go
  (when (and acm-preview-overlay
             (not (string-prefix-p "acm" (lsp-bridge-command-name))))
    (acm-complete))

  (when acm-filter-overlay
    (let ((this-command-string (lsp-bridge-command-name)))
      (cond ((member this-command-string '("self-insert-command" "org-self-insert-command"))
             (setq this-command 'acm-filter-insert-char)
             (setq last-command 'acm-filter-insert-char))
            ((member this-command-string '("delete-block-backward" "grammatical-edit-backward-delete"))
             (setq this-command 'acm-filter-delete-char)
             (setq last-command 'acm-filter-delete-char))))))

(defun lsp-bridge-monitor-post-command ()
  "React to the command that just ran.
Runs in `post-command-hook': it records the cursor position, tries
completion after insert commands, sends `change_cursor' and
`document_symbol' requests when the cursor moved, and hides the
documentation, diagnostic, signature and code action popups."
  (setq-local lsp-bridge-cursor-after-command (point))

  (let ((this-command-string (lsp-bridge-command-name)))
    (when (and lsp-bridge-mode
               (member this-command-string '("self-insert-command" "org-self-insert-command" "lsp-bridge-popup-complete-menu")))
      (lsp-bridge-try-completion))

    (when (lsp-bridge-has-lsp-server-p)
      (unless (equal lsp-bridge-last-cursor-position
                     (setq-local lsp-bridge-last-cursor-position (point)))
        ;; Only show hover when cursor move.
        (when (and lsp-bridge-enable-hover-diagnostic
                   (not (member this-command-string
                                '("self-insert-command" "org-self-insert-command"
                                  "lsp-bridge-diagnostic-jump-next" "lsp-bridge-diagnostic-jump-prev"))))
          (lsp-bridge-diagnostic-maybe-display-error-at-point))

        ;; Only send `change_cursor' request when user change cursor, except cause by mouse wheel.
        (unless (eq last-command 'mwheel-scroll)
          (lsp-bridge-call-file-api "change_cursor" (lsp-bridge--position))
          (if (and lsp-bridge-symbols-enable-which-func
                   (featurep 'which-func) which-function-mode)
              (lsp-bridge-call-file-api "document_symbol" (lsp-bridge--position)))))

      ;; Hide hover tooltip.
      (unless (string-prefix-p "lsp-bridge-popup-documentation-scroll" this-command-string)
        (lsp-bridge-hide-doc-tooltip))

      ;; Hide diagnostic tooltip.
      (unless (member this-command-string '("lsp-bridge-diagnostic-jump-next"
                                            "lsp-bridge-diagnostic-jump-prev"))
        (lsp-bridge-diagnostic-hide-tooltip))

      ;; Hide signature tooltip.
      (lsp-bridge-hide-signature-tooltip)

      ;; Hide code action frame when Emacs got focus.
      (unless (string-prefix-p "lsp-bridge-code-action" this-command-string)
        (unless (member this-command-string '("handle-switch-frame"))
          (ignore-errors
            (lsp-bridge-code-action-popup-quit))))

      ;; Try send inlay hint if window scroll.
      (lsp-bridge-inlay-hint-monitor-window-scroll))))

(defun lsp-bridge-close-buffer-file ()
  "Tell the server that this buffer file was closed.
Remote files get a `close_file' request, local files a `close_file'
and a `search_file_words_close_file' request."
  (if (lsp-bridge-is-remote-file)
      (progn
        (lsp-bridge-remote-send-func-request "close_file" (list lsp-bridge-remote-file-path))
        (lsp-bridge-remote-send-func-request "search_file_words_close_file" (list lsp-bridge-remote-file-path)))

    (when (lsp-bridge-process-live-p)
      (when (and (lsp-bridge-has-lsp-server-p)
                 (boundp 'acm-backend-lsp-filepath))
        (lsp-bridge-call-async "close_file" acm-backend-lsp-filepath))

      (when buffer-file-name
        (lsp-bridge-call-async "search_file_words_close_file" (lsp-bridge-get-buffer-file-name-text))))))

(defun lsp-bridge--point-position (pos)
  "Get position of POS."
  (save-excursion
    (goto-char pos)
    (lsp-bridge--position)))

(defun lsp-bridge--calculate-column ()
  "Calculate character offset of cursor in current line."
  (/ (- (length (encode-coding-region (line-beginning-position)
                                      (min (point) (point-max)) 'utf-16 t))
        2)
     2))

(defun lsp-bridge--position ()
  "Get position of cursor."
  ;; we should use ABSOLUTE line number to be compatible with narrowed buffer
  (list :line (1- (line-number-at-pos nil t))
        :character (lsp-bridge--calculate-column)))

(defun lsp-bridge--position-in-org ()
  "Get position in org source block.
Return a plist with :line and :character.
The line number is relative to the beginning of the source block."
  (if (and (derived-mode-p 'org-mode)
           (org-in-src-block-p))
      (let* ((element (org-element-context))
             (begin-pos (plist-get (car (cdr element)) :begin))
             (current-line (line-number-at-pos nil t))
             (block-line (line-number-at-pos begin-pos t))
             (relative-line (max 0 (- current-line block-line 1))))
        (list :line relative-line
              :character (lsp-bridge--calculate-column)))
    (lsp-bridge--position)))

(defvar-local lsp-bridge--before-change-begin-pos nil
  "LSP position of the beginning of the last recorded change.")

(defvar-local lsp-bridge--before-change-end-pos nil
  "LSP position of the end of the last recorded change.")

(defvar-local lsp-bridge--before-change-begin-point nil
  "Buffer position of the beginning of the last recorded change.")

(defvar-local lsp-bridge--before-change-end-point nil
  "Buffer position of the end of the last recorded change.")

(defun lsp-bridge-monitor-before-change (begin end)
  "Record the buffer state before a change from BEGIN to END.
Runs in `before-change-functions': it sends the current org src block
to the server when appropriate and, for buffers with an LSP server or
for remote files, saves the change bounds and their LSP positions in
the `lsp-bridge--before-change-*' buffer-local variables."
  ;; Use `save-match-data' protect match data, avoid conflict with command call `search-regexp'.
  (save-restriction
    (widen)
    (save-match-data
      (when (lsp-bridge-has-lsp-server-p)
        ;; Send whole org src block to lsp server.
        (lsp-bridge-org-babel-send-src-block-to-lsp-server))

      ;; Set `lsp-bridge--before-change-begin-pos' and `lsp-bridge--before-change-end-pos'
      ;; if `lsp-bridge-has-lsp-server-p' or `lsp-bridge-is-remote-file'
      (when (or (lsp-bridge-has-lsp-server-p)
                (lsp-bridge-is-remote-file))
        (setq-local lsp-bridge--before-change-begin-point begin)
        (setq-local lsp-bridge--before-change-end-point end)

        (setq-local lsp-bridge--before-change-begin-pos (lsp-bridge--point-position begin))
        (setq-local lsp-bridge--before-change-end-pos (lsp-bridge--point-position end))))))

(defun lsp-bridge-monitor-post-self-insert ()
  "Record the position after a self insert finished.
Runs in `post-self-insert-hook', after pairing modes such as
`electric-pair-mode' and `smartparens-mode' ran, so that the recorded
position is the final one and `lsp-bridge-try-completion' does not
misjudge it."
  ;; Make sure this function be called after `electric-pair-mode'
  ;; `smartparens-mode' or something similar if they are enabled.
  ;;
  ;; Because pairing parethness or quotes will call `after-change-functions'
  ;; multiple times. For example: (| represents the cursor)
  ;;
  ;;         dic|
  ;;
  ;; Press `[', then the following situation will be observed in
  ;; `lsp-bridge-monitor-after-change':
  ;;
  ;;         dic[|
  ;;         dic|
  ;;         dic[|
  ;;         dic[]|
  ;;
  ;; The last position of cursor is wrong, that makes a misjudgment in
  ;; `lsp-bridge-try-completion'.
  (lsp-bridge-record-last-change-position))

(defun lsp-bridge-monitor-after-change (begin end length)
  "Send LSP requests for the change from BEGIN to END of LENGTH.
Runs in `after-change-functions'.  The change is ignored when it only
repeats the bounds recorded by `lsp-bridge-monitor-before-change' or
while the buffer is being reverted; otherwise the last command and
change position are recorded and `change_file', inlay hint, other
backends and search words requests are triggered."
  ;; Nothing change actual if `begin' and `end' equal `lsp-bridge--before-change-begin-point' and `lsp-bridge--before-change-end-point'
  ;; Then we should not send any request to search backend.
  (when (lsp-bridge-process-live-p)
    (unless (and (equal begin lsp-bridge--before-change-begin-point)
                 (equal end lsp-bridge--before-change-end-point))
      ;; Use `save-match-data' protect match data, avoid conflict with command call `search-regexp'.
      (save-restriction
        (widen)
        (save-match-data
          (unless lsp-bridge-revert-buffer-flag
            (let ((change-text (buffer-substring-no-properties begin end)))
              ;; Record last command to `lsp-bridge-last-change-command'.
              (setq lsp-bridge-last-change-command (lsp-bridge-command-name))

              ;; Record last change position to avoid popup outdate completions.
              (lsp-bridge-record-last-change-position)

              ;; Set `lsp-bridge-last-change-is-delete-command-p'
              (setq lsp-bridge-last-change-is-delete-command-p (> length 0))

              ;; Sync change for org babel if we enable it
              (lsp-bridge-org-babel-monitor-after-change begin end length)

              ;; Send LSP requests.
              (when (or (lsp-bridge-call-file-api-p)
                        (lsp-bridge-is-remote-file))

                ;; Uncomment below code to debug `change_file' protocol.
                ;; (message (format "change_file: '%s' '%s' '%s' '%s' '%s' '%s'"
                ;;                  length
                ;;                  lsp-bridge--before-change-begin-pos
                ;;                  lsp-bridge--before-change-end-pos
                ;;                  (lsp-bridge--position)
                ;;                  change-text
                ;;                  (buffer-substring-no-properties (line-beginning-position) (point))
                ;;                  ))

                ;; Send change_file request to trigger LSP completion.
                (lsp-bridge-call-file-api "change_file"
                                          lsp-bridge--before-change-begin-pos
                                          lsp-bridge--before-change-end-pos
                                          length
                                          change-text
                                          (lsp-bridge--position)
                                          (acm-char-before)
                                          (buffer-name)
                                          (acm-get-input-prefix))

                ;; Send inlay hint request.
                (lsp-bridge-inlay-hint-try-send-request))

              ;; Complete other non-LSP backends.
              (lsp-bridge-complete-other-backends)

              ;; Update search words backend.
              ;;
              ;; disable it for org-mode when we enable `lsp-bridge-enable-org-babel'
              ;; it will trigger with wrong pos due to we only update `pos' on src block
              (unless (eq major-mode 'org-mode)
                (lsp-bridge-search-words-update
                 lsp-bridge--before-change-begin-pos
                 lsp-bridge--before-change-end-pos
                 change-text)))))))))

(defun lsp-bridge-elisp-get-filepath ()
  "Return the file path at point.
Paths that contain spaces are supported."
  (let* ((file-end (point))
         (filepath (save-excursion
                     (catch 'break
                       (let* ((file-path "")
                              (file-beg 0))
                         (while (acm-in-string-p)
                           (setq file-beg (car (bounds-of-thing-at-point 'filename)))
                           (if file-beg
                               (progn
                                 (setq file-path (buffer-substring file-beg file-end))
                                 (if (and (file-name-directory file-path) (file-exists-p (file-name-directory file-path)))
                                     (progn
                                       (throw 'break file-path))
                                   (if (eq file-beg 1)
                                       ;; Reached beginning of buffer
                                       (throw 'break nil)
                                     (goto-char (1- file-beg)))))
                             (throw 'break nil))))))))
    filepath))

(defun lsp-bridge-elisp-symbols-update ()
  "We need synchronize elisp symbols to Python side when idle."
  (when (lsp-bridge-process-live-p)
    (let* ((symbols (acm-backend-elisp-get-symbols))
           (symbols-size (length symbols)))
      ;; Only synchronize when new symbol created.
      (unless (equal acm-backend-elisp-symbols-update-size symbols-size)
        (lsp-bridge-call-async "search_list_update"
                               "elisp"
                               symbols
                               acm-backend-elisp-search-max-number)
        (setq acm-backend-elisp-symbols-update-size symbols-size)))))

(defun lsp-bridge-elisp-symbols-search (current-symbol)
  "Search elisp symbols matching CURRENT-SYMBOL.
Skip the search when the buffer is not an elisp buffer, when point is
in a comment, or when CURRENT-SYMBOL is empty; the search runs in the
elisp search backend on the Python side."
  (when (and (acm-is-elisp-mode-p)
             (not (acm-in-comment-p)))
    ;; Search words if current prefix is not empty.
    (unless (or (string-equal current-symbol "") (null current-symbol))
      (lsp-bridge-call-async "search_list_search" "elisp" current-symbol))))

(defun lsp-bridge-search-words-index-files ()
  "Index files when lsp-bridge python process finish."
  (when lsp-bridge-enable-search-words
    (if (lsp-bridge-is-remote-file)
        (let* ((host lsp-bridge-remote-file-host)
               (buffers (cl-remove-if-not (lambda (buf)
                                            (with-current-buffer buf
                                              (and (lsp-bridge-is-remote-file)
                                                   (string-equal lsp-bridge-remote-file-host host))))
                                          (buffer-list)))
               (files (mapcar (lambda (buf)
                                (with-current-buffer buf
                                  lsp-bridge-remote-file-path))
                              buffers)))
          (lsp-bridge-remote-send-func-request "search_file_words_index_files" (list files)))
      (let ((files (cl-remove-if (lambda (elt)
                                   (or (null elt)
                                       (file-remote-p elt)
                                       (member (file-name-extension elt)
                                               lsp-bridge-search-words-prohibit-file-extensions)))
                                 (mapcar (lambda (b) (lsp-bridge-buffer-file-name (buffer-file-name b))) (buffer-list)))))
        (lsp-bridge-call-async "search_file_words_index_files" files)))))

(defun lsp-bridge-search-words-update (begin-pos end-pos change-text)
  "Update the search words backend for a change of CHANGE-TEXT.
BEGIN-POS and END-POS delimit the change in LSP positions.  Remote
files reload their words from the remote server, local files send a
`search_file_words_change_buffer' request for the current buffer."
  (if (lsp-bridge-is-remote-file)
      (progn
        (lsp-bridge-remote-send-func-request "search_file_words_load_file" (list lsp-bridge-remote-file-path t)))
    (when (lsp-bridge-process-live-p)
      (lsp-bridge-call-async "search_file_words_change_buffer"
                             (substring-no-properties (buffer-name))
                             begin-pos
                             end-pos
                             change-text))))

(defun lsp-bridge-monitor-after-save ()
  "Notify the server that the current buffer was saved.
Runs in `after-save-hook' and sends a `save_file' request with the
buffer name."
  (lsp-bridge-call-file-api "save_file" (buffer-name)))

(defvar lsp-bridge--last-buffer nil
  "Buffer that was current during the previous window change check.")

(defvar-local lsp-bridge--work-done-progress nil
  "Buffer local LSP work-done progress message for the mode line.")

(defvar-local lsp-bridge--work-done-progress-timer nil
  "Buffer-local Timer to clear `lsp-bridge--work-done-progress' after inactivity.")

(defun lsp-bridge-monitor-window-buffer-change ()
  "Hide the lsp-bridge popups when the current buffer changed.
Runs in `post-command-hook': it hides the documentation, diagnostic
and signature popups and records the new current buffer unless the
minibuffer or the `*Messages*' buffer is current."
  ;; Hide completion, diagnostic and signature frame when buffer or window changed.
  (unless (eq (current-buffer)
              lsp-bridge--last-buffer)
    (lsp-bridge-hide-doc-tooltip)
    (lsp-bridge-diagnostic-hide-tooltip)
    (lsp-bridge-hide-signature-tooltip))

  (unless (or (minibufferp)
              (string-equal (buffer-name) "*Messages*"))
    (setq lsp-bridge--last-buffer (current-buffer))))

(defun lsp-bridge-enable-in-minibuffer ()
  "Enable `lsp-bridge-mode' in a minibuffer that completes.
Runs in `minibuffer-setup-hook' when
`lsp-bridge-enable-completion-in-minibuffer' is non-nil and the local
map provides a `completion-at-point' binding."
  (when (and lsp-bridge-enable-completion-in-minibuffer
             (where-is-internal #'completion-at-point (list (current-local-map))))
    (lsp-bridge-mode 1)))

(defconst lsp-bridge--internal-hooks
  '((before-change-functions lsp-bridge-monitor-before-change nil t)
    (after-change-functions lsp-bridge-monitor-after-change nil t)
    (pre-command-hook lsp-bridge-monitor-pre-command nil t)
    (post-command-hook lsp-bridge-monitor-post-command nil t)
    (after-save-hook lsp-bridge-monitor-after-save nil t)
    (kill-buffer-hook lsp-bridge-close-buffer-file nil t)
    (before-revert-hook lsp-bridge-close-buffer-file nil t)
    (post-self-insert-hook lsp-bridge-monitor-post-self-insert 90 t))
  "Hooks added buffer-locally while lsp-bridge mode is enabled.
Each element is a hook list in the form accepted by `add-hook'.")

(defvar lsp-bridge-mode-map (make-sparse-keymap)
  "Keymap used by `lsp-bridge-mode'.")

(defvar lsp-bridge-search-words-timer nil
  "Idle timer that updates the search words backend.")

(defvar lsp-bridge-auto-format-code-timer nil
  "Idle timer that triggers `lsp-bridge-auto-format-code'.")

(defcustom lsp-bridge-mode-lighter " LSPB"
  "Mode line lighter for LSP Bridge Mode."
  :type 'string
  :group 'lsp-bridge)


(defun lsp-bridge--enable ()
  "Enable LSP Bridge mode."

  (add-hook 'post-command-hook #'lsp-bridge-start-process)

  ;; Don't enable lsp-bridge when the current buffer is a popup of the menu.
  ;; `acm-session' is what knows the popup buffers, and this file is the leaf
  ;; that the rest of the package loads first, so it is not required here: when
  ;; it is not loaded (an older acm, a config that loads core on its own) there
  ;; is no popup buffer either, and the answer is nil.  Calling it unguarded made
  ;; enabling the mode depend on something this file does not control.
  (unless (and (fboundp 'acm-session-popup-buffer-p)
               (acm-session-popup-buffer-p))
    ;; Disable backup file.
    ;; Please use my another plugin `https://github.com/manateelazycat/auto-save' and use git for file version management.
    (when lsp-bridge-disable-backup
      (setq make-backup-files nil)
      (setq auto-save-default nil)
      (setq create-lockfiles nil))

    ;; Add `lsp-bridge-symbols--current-defun' to `whic-func-functions'.
    (if (and lsp-bridge-symbols-enable-which-func
             (featurep 'which-func) which-function-mode)
        (setq-local which-func-functions
                    (add-to-list 'which-func-functions #'lsp-bridge-symbols--current-defun)))

    (setq-local lsp-bridge-revert-buffer-flag nil)

    (acm-run-idle-func acm-backend-elisp-symbols-update-timer lsp-bridge-elisp-symbols-update-idle 'lsp-bridge-elisp-symbols-update)

    (when (or (lsp-bridge-has-lsp-server-p)
              ;; Init acm backend for org babel, and need elimination org temp buffer.
              (and lsp-bridge-enable-org-babel
                   (eq major-mode 'org-mode)
                   (not (lsp-bridge-is-org-temp-buffer-p))))
      ;; When user open buffer by `ido-find-file', lsp-bridge will throw `FileNotFoundError' error.
      ;; So we need save buffer to disk before enable `lsp-bridge-mode'.
      (when (not (lsp-bridge-is-remote-file))
        (when (and (buffer-file-name)
                   (not (file-exists-p (buffer-file-name))))
          (save-buffer)))

      ;; Init backends.
      (acm-backend-lsp-init)
      (acm-backend-lsp-workspace-symbol-init)

      (when lsp-bridge-enable-signature-help
        (acm-run-idle-func lsp-bridge-signature-help-timer lsp-bridge-signature-help-fetch-idle 'lsp-bridge-signature-help-fetch))
      (when lsp-bridge-enable-auto-format-code
        (acm-run-idle-func lsp-bridge-auto-format-code-timer lsp-bridge-auto-format-code-idle 'lsp-bridge-auto-format-code))

      (when lsp-bridge-enable-document-highlight
        (lsp-bridge-enable-document-highlight-timer)
        (lsp-bridge-document-highlight-setup)))

    (dolist (hook lsp-bridge--internal-hooks)
      (apply #'add-hook hook))

    (advice-add #'acm-hide :after #'lsp-bridge--completion-hide-advisor)))

(defun lsp-bridge-is-org-temp-buffer-p ()
  "Return non-nil if the current buffer is an org temporary buffer.
Such buffers are created by org fontification, org src editing or
capture and must not start lsp-bridge."
  (or (string-match "\*org-src-fontification:" (buffer-name))
      (string-match "\*Org Src" (buffer-name))
      (string-match ".org-src-babel" (buffer-name))
      (equal "*Capture*" (buffer-name))))

(defun lsp-bridge--disable ()
  "Disable LSP Bridge mode."
  ;; Remove hooks.
  (dolist (hook lsp-bridge--internal-hooks)
    (remove-hook (nth 0 hook) (nth 1 hook) (nth 3 hook)))

  ;; Cancel idle timer.
  (acm-cancel-timer lsp-bridge-signature-help-timer)
  (acm-cancel-timer lsp-bridge-search-words-timer)
  (acm-cancel-timer lsp-bridge-auto-format-code-timer)
  (acm-cancel-timer acm-backend-elisp-symbols-update-timer)

  ;; Reset `acm-backend-elisp-symbols-update-size' to zero.
  (setq acm-backend-elisp-symbols-update-size 0)

  ;; Remove hide advice.
  (advice-remove #'acm-hide #'lsp-bridge--completion-hide-advisor))

(defun lsp-bridge--turn-off-lsp-feature (filename filehost)
  "Mark the buffer of FILENAME on FILEHOST as having no LSP server.
Sets its buffer-local `acm-backend-lsp-server-command-exist' to nil
so that no further file request is sent."
  (lsp-bridge--with-file-buffer filename filehost
                                (setq-local acm-backend-lsp-server-command-exist nil)))


(defun lsp-bridge--rename-file-advisor (orig-fun &optional arg &rest args)
  "Tell the server about a rename before `rename-file' runs.
ORIG-FUN, ARG and ARGS describe the advised call.  When the renamed
file is the file of this buffer a `rename_file' request is sent, the
old file is closed and the buffer file name is updated."
  (let* ((current-file-name (buffer-file-name))
         (current-file-name (and current-file-name
                                 (expand-file-name current-file-name)))
         (new-name (expand-file-name (nth 0 args))))
    (when (and lsp-bridge-mode
               (boundp 'acm-backend-lsp-filepath)
               current-file-name
               (string-equal current-file-name new-name))
      (lsp-bridge-call-file-api "rename_file" new-name)
      (if (lsp-bridge-is-remote-file)
          (lsp-bridge-remote-send-func-request "close_file" (list acm-backend-lsp-filepath))
        (lsp-bridge-call-async "close_file" acm-backend-lsp-filepath))
      (set-visited-file-name new-name t t)
      (setq-local acm-backend-lsp-filepath new-name)))
  (apply orig-fun arg args))

(defun lsp-bridge--cleanup ()
  "Remove the lsp-bridge overlays before the buffer changes.
Hides the diagnostic and inlay hint overlays."
  ;; We need clean diagnostic overlays before revert buffer.
  (lsp-bridge-diagnostic-hide-overlays)

  ;; Hide inlay hint overlays before revert buffer.
  (lsp-bridge-inlay-hint-hide-overlays))

;; We use `lsp-bridge-revert-buffer-flag' var avoid lsp-bridge send change_file request while execute `revert-buffer' command.
(defun lsp-bridge--revert-buffer-advisor (orig-fun &optional arg &rest args)
  "Clean up lsp-bridge overlays around a buffer revert.
ORIG-FUN, ARG and ARGS describe the advised call; the
`lsp-bridge-revert-buffer-flag' buffer-local variable is set while
the revert runs so that no `change_file' request is sent for it."
  ;; Clean up.
  (lsp-bridge--cleanup)

  (setq-local lsp-bridge-revert-buffer-flag t)
  (apply orig-fun arg args)
  (setq-local lsp-bridge-revert-buffer-flag nil))

(defun lsp-bridge--completion-hide-advisor (&rest args)
  "Reset the resolved completion item ticker when the menu hides.
ARGS are ignored; runs as :after advice on `acm-hide' so the next menu
resolves its items again."
  (when lsp-bridge-mode
    ;; Clean LSP backend completion tick.
    (setq-local acm-backend-lsp-fetch-completion-item-ticker nil)))

(provide 'lsp-bridge-core)

;;; lsp-bridge-core.el ends here
