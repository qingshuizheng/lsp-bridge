;;; lsp-bridge-workspace.el --- Workspace symbols, formatting, edits, indent and document highlight -*- lexical-binding: t -*-

;;; Commentary:

;; Part of lsp-bridge, loaded by lsp-bridge.el at the end of its own load.
;; Symbols that live in the other files of the package are declared below rather
;; than required, so that the files never need each other; a macro is autoloaded
;; with the `macro' flag, because a macro that the compiler does not know is
;; compiled as a function call.  Only libraries outside the package are
;; required.  The byte-compile gate reports a symbol that is used and not
;; declared, which keeps these lists honest.

;;; Code:

(declare-function acm-backend-lsp-apply-text-edits "acm-backend-lsp")
(declare-function acm-backend-lsp-position-to-point "acm-backend-lsp")
(declare-function lsp-bridge--get-indent-width "lsp-bridge")
(declare-function lsp-bridge--point-position "lsp-bridge")
(declare-function lsp-bridge--position "lsp-bridge")
(require 'lsp-bridge-core)

;; The macros of `lsp-bridge-core' expand into calls to these, which this
;; file does not define.
(declare-function lsp-bridge--resolve-remote-host "lsp-bridge")
(declare-function lsp-bridge-get-match-buffer-by-filepath "lsp-bridge")
(declare-function lsp-bridge-get-match-buffer-by-remote-file "lsp-bridge")
(declare-function lsp-bridge-call-async "lsp-bridge")
(declare-function lsp-bridge-call-file-api "lsp-bridge")
(declare-function lsp-bridge-completion-ui-visible-p "lsp-bridge")
(declare-function lsp-bridge-has-lsp-server-p "lsp-bridge")
(declare-function lsp-bridge-is-remote-file "lsp-bridge")
(defvar lsp-bridge-mode)
(defvar lsp-bridge-prohibit-completion)
(defvar lsp-bridge-remote-file-host)
(defvar lsp-bridge-remote-file-path)
(autoload 'lsp-bridge-save-position "lsp-bridge" nil nil 'macro)
(declare-function lsp-bridge-try-completion "lsp-bridge")
(defvar lsp-bridge-workspace-symbol-kind-to-face)

(defun lsp-bridge-completion-workspace-symbol--record-items (filename
                                                             filehost
                                                             candidates
                                                             server-name
                                                             server-names)
  "Record workspace symbol CANDIDATES for the buffer of FILENAME.
FILEHOST locates the buffer, SERVER-NAME names the answering server
and SERVER-NAMES is stored for later requests.  The candidates are
cached in the buffer and the completion menu is tried immediately."
  ;; Adjust `gc-cons-threshold' to maximize temporary,
  ;; make sure Emacs not do GC
  (let ((gc-cons-threshold most-positive-fixnum))
    (lsp-bridge--with-file-buffer filename filehost
                                  ;; Save completion items.
                                  (setq-local acm-backend-lsp-workspace-symbol-cache-candidates nil)
                                  (setq-local acm-backend-lsp-workspace-symbol-server-names server-names)

                                  (let* ((lsp-items acm-backend-lsp-workspace-symbol-items)
                                         (completion-table (make-hash-table :test 'equal)))
                                    (dolist (item candidates)
                                      (plist-put item :annotation (capitalize (plist-get item :icon)))
                                      (puthash (plist-get item :key) item completion-table))
                                    (puthash server-name completion-table lsp-items)
                                    (setq-local acm-backend-lsp-workspace-symbol-items lsp-items))
                                  (lsp-bridge-try-completion))))

(defun lsp-bridge-pick-file-path (filename)
  "Return the local file path described by the LSP FILENAME.
A leading `file://' or `:file://' is removed, `%XX' sequences are
decoded and the leading slash before a Windows drive letter is
dropped."
  ;; Remove `file://' and `:file://' prefix.
  (cond ((string-prefix-p "file://" filename)
         (setq filename (string-remove-prefix "file://" filename)))
        ((string-prefix-p ":file://" filename)
         (setq filename (string-remove-prefix ":file://" filename))))

  ;; Convert `%XX' sequences to `:'
  (setq filename (url-unhex-string filename))

  ;; Remove / before drive letter on Windows
  (when (string-match "^/[A-Za-z]:" filename)
    (setq filename (substring filename 1)))

  filename)

(defun lsp-bridge-file-apply-edits (filename edits &optional temp-buffer)
  "Apply EDITS to the file FILENAME.
With TEMP-BUFFER the edits are applied in that buffer, otherwise the
file is visited and the edits applied there.  Completion is prohibited
in the buffer afterwards."
  (if temp-buffer
      ;; Apply edits to temp buffer.
      (with-current-buffer temp-buffer
        (acm-backend-lsp-apply-text-edits edits))

    ;; Pick filename from LSP return file string.
    (setq filename (lsp-bridge-pick-file-path filename))

    (find-file-noselect filename)
    (save-excursion
      (find-file filename)
      (acm-backend-lsp-apply-text-edits edits)))

  (setq-local lsp-bridge-prohibit-completion t))

(defun lsp-bridge-workspace-list-symbol-at-point ()
  "List workspace symbols matching the symbol or region at point.
The active region text is used as query when there is one, otherwise
the symbol at point is searched."
  (interactive)
  (if (region-active-p)
      (lsp-bridge-workspace-list-symbols (buffer-substring-no-properties (mark) (point)))
    (lsp-bridge-workspace-list-symbols (substring-no-properties (symbol-name (symbol-at-point))))))

(defun lsp-bridge-workspace-list-symbols (query)
  "Ask the server for workspace symbols matching QUERY.
QUERY is read from the minibuffer when the command is called
interactively; the result arrives in
`lsp-bridge-workspace--list-symbols'."
  (interactive "sWorkspace symbol query: ")
  (lsp-bridge-call-file-api "workspace_symbol" query))

(defun lsp-bridge-workspace--list-symbols (info)
  "Let the user pick one of the workspace symbols in INFO.
INFO is the list of symbol plists from the server; the chosen symbol
is opened at its location through `lsp-bridge-jump-to-file', and a
message is shown when the server returned no symbol."
  (if (zerop (length info))
      (message "LSP server did not return any symbols.")
    (let* ((symbols (mapcar #'lsp-bridge-workspace-transform-info info))
           (match-symbol (completing-read "Workspace symbol: " symbols))
           (match-info (seq-filter (lambda (i) (string-equal match-symbol (lsp-bridge-workspace-transform-info i))) info)))
      (when match-info
        (let* ((symbol-info (plist-get (car match-info) :location))
               (symbol-file (lsp-bridge-pick-file-path (format "%s" (plist-get symbol-info :uri))))
               (symbol-position (plist-get (plist-get symbol-info :range) :start)))
          (lsp-bridge-jump-to-file symbol-file symbol-position))))))

(defun lsp-bridge-jump-to-file (file position)
  "Open FILE and move to POSITION.
Virtual schemes such as jdt, deno and csharp metadata are resolved by
the server first; ordinary files are visited directly and POSITION is
converted to a buffer position."
  (cond ((string-prefix-p "jdt://" file)
         (lsp-bridge-call-file-api "jdt_uri_resolver" (url-encode-url file) position "lsp-bridge-define--jump"))
        ((string-prefix-p "deno:" file)
         (lsp-bridge-call-file-api "deno_uri_resolver" (url-encode-url file) position "lsp-bridge-define--jump"))
        ((string-prefix-p "csharp:/metadata/" file)
         (lsp-bridge-call-file-api "csharp_uri_resolver" (url-encode-url file) position "lsp-bridge-define--jump"))
        (t
         (find-file file)
         (goto-char (acm-backend-lsp-position-to-point position)))))

(defun lsp-bridge-workspace-transform-info (info)
  "Return a completion string for the workspace symbol INFO.
The string is prefixed with the colored symbol kind and suffixed with
the file path of the symbol relative to the project root."
  (let* ((name (plist-get info :name))
         (uri (plist-get (plist-get info :location) :uri))
         (kind (plist-get info :kind))
         (sanitized-kind (if (< kind (length lsp-bridge-workspace-symbol-kind-to-face)) kind 0))
         (type (elt lsp-bridge-workspace-symbol-kind-to-face sanitized-kind))
         (typestr (propertize (format "[%s] " (car type)) 'face (cdr type)))
         (project-root (locate-dominating-file default-directory ".git"))
         (pathstr (propertize (format " · %s" (file-relative-name (substring uri 7 nil) project-root))
                              'face font-lock-comment-face)))
    (concat typestr name pathstr)))

(defun lsp-bridge-auto-format-code ()
  "Format the current buffer when the idle timer fires.
Formatting is skipped while the mouse wheel scrolls."
  (unless (eq last-command 'mwheel-scroll)
    (lsp-bridge-code-format)))

(defun lsp-bridge-code-format ()
  "Ask the server to format the region or the current line.
The request is skipped unless a language server applies, the
completion menu is hidden and neither yasnippet nor tempel has an
active snippet; the major mode indentation width is sent along."
  (interactive)
  (when (and
         ;; Current buffer has LSP server.
         (lsp-bridge-has-lsp-server-p)
         ;; Completion menu not show.
         (not (lsp-bridge-completion-ui-visible-p))
         ;; Yasnippet not active.
         (or (not (boundp 'yas--active-snippets))
             (not yas--active-snippets))
         ;; Tempel not active.
         (or (not (boundp 'tempel--active))
             (not tempel--active)))
    (let* ((indent (symbol-value (lsp-bridge--get-indent-width major-mode)))
           (start (lsp-bridge--point-position (if (region-active-p) (region-beginning) (point))))
           (end (lsp-bridge--point-position (if (region-active-p) (region-end) (point)))))
      (lsp-bridge-call-file-api "try_formatting"
                                start
                                end
                                ;; Sometimes `c-basic-offset' return string `set-from-style', make some lsp server broken, such as, gopls,
                                ;; so we need convert indent to integer `4' to make sure code format works expectantly.
                                (if (eq indent 'set-from-style)
                                    4
                                  indent)))))

(defun lsp-bridge-format--update (filename edits)
  "Apply the formatting EDITS to FILENAME.
The edits are applied with modification hooks inhibited and the
original position is restored; remote files are uploaded again and the
server is told that the whole file changed."
  ;; We need set `inhibit-modification-hooks' to t to avoid GC freeze Emacs.
  (lsp-bridge-save-position
   (let ((inhibit-modification-hooks t))
     ;; Apply code format edits, not sort, just reverse order.
     (lsp-bridge-file-apply-edits filename edits)
     (when (lsp-bridge-is-remote-file)
       (lsp-bridge-call-async "update_remote_file"
                              lsp-bridge-remote-file-host
                              lsp-bridge-remote-file-path
                              (buffer-substring-no-properties (point-min) (point-max))))
     ;; Make LSP server update full content.
     (lsp-bridge-call-file-api "update_file" (buffer-name))
     ;; Notify format complete.
     (message "[LSP-BRIDGE] Complete code formatting."))))

(defun lsp-bridge-workspace-apply-edit (info &optional temp-buffer)
  "Apply the workspace edit described by INFO.
Both the `documentChanges' and the `changes' shape are accepted; with
TEMP-BUFFER the edits go to that buffer.  Completion is prohibited in
the buffer afterwards and the original position is restored."
  (lsp-bridge-save-position
   (cond ((plist-get info :documentChanges)
          (dolist (change (plist-get info :documentChanges))
            (lsp-bridge-file-apply-edits
             (plist-get (plist-get change :textDocument) :uri)
             (plist-get change :edits)
             temp-buffer)))
         ((plist-get info :changes)
          (let* ((changes (plist-get info :changes))
                 (changes-number (/ (length changes) 2)))
            (dotimes (changes-index changes-number)
              (lsp-bridge-file-apply-edits
               (format "%s" (nth (* changes-index 2) changes))
               (nth (+ (* changes-index 2) 1) changes) temp-buffer))))))

  (setq-local lsp-bridge-prohibit-completion t))

(defun lsp-bridge-indent-left (start end)
  "Indent the region from START to END to the left by one level.
The indentation width comes from `lsp-bridge--get-indent-width' of
the major mode."
  (interactive "r")
  (let ((indent (symbol-value (lsp-bridge--get-indent-width major-mode))))
    (indent-rigidly start end (- indent))))

(defun lsp-bridge-indent-right (start end)
  "Indent the region from START to END to the right by one level.
The indentation width comes from `lsp-bridge--get-indent-width' of
the major mode."
  (interactive "r")
  (let ((indent (symbol-value (lsp-bridge--get-indent-width major-mode))))
    (indent-rigidly start end indent)))

(defcustom lsp-bridge-enable-document-highlight nil
  "Enable document highlight."
  :type 'boolean
  :group 'lsp-bridge)

(defvar lsp-bridge-document-highlight-timer nil
  "Idle timer that triggers `lsp-bridge-document-highlight'.")

(defun lsp-bridge-document-highlight ()
  "Ask the server for the highlights of the symbol at point.
The request is skipped when lsp-bridge mode or document highlighting
is off, or while the minibuffer is active."
  (when (and lsp-bridge-mode
             lsp-bridge-enable-document-highlight
             (not (minibufferp)))
    (lsp-bridge-call-file-api "document_highlight" (lsp-bridge--position))))

(defvar lsp-bridge-document-highlight-hook nil
  "Hook run with the highlight list of a `document_highlight' answer.
The list is the one `lsp-bridge-document-highlight-render' is about to draw,
which is what lets a feature use the ranges for something else as well.")

(defun lsp-bridge-document-highlight-render (highlights)
  "Draw an overlay for every range in HIGHLIGHTS.
Existing highlight overlays are removed first and each overlay uses
`lsp-bridge-document-highlight-face' in the selected window.  Run
`lsp-bridge-document-highlight-hook' first so that the answer can be used
before the overlays are drawn."
  (run-hook-with-args 'lsp-bridge-document-highlight-hook highlights)
  (remove-overlays (point-min) (point-max) 'face 'lsp-bridge-document-highlight-face)
  (dolist (highlight highlights)
    (let* ((range (plist-get highlight :range))
           (start (plist-get range :start))
           (end (plist-get range :end))
           (start-line (plist-get start :line))
           (start-character (plist-get start :character))
           (end-line (plist-get end :line))
           (end-character (plist-get end :character))
           (start-pos (lsp-bridge-document-highlight--position-to-point start-line start-character))
           (end-pos (lsp-bridge-document-highlight--position-to-point end-line end-character))
           (overlay (make-overlay start-pos end-pos)))
      (overlay-put overlay 'face 'lsp-bridge-document-highlight-face)
      (overlay-put overlay 'window (selected-window)))))

(defun lsp-bridge-document-highlight--position-to-point (line character)
  "Convert the LSP position LINE and CHARACTER to a buffer position."
  (save-excursion
    (goto-char (point-min))
    (forward-line line)
    (forward-char character)
    (point)))

(defface lsp-bridge-document-highlight-face
  '((t :inherit highlight :background "gray20"))
  "Face used for document highlight overlays."
  :group 'lsp-bridge)

(defun lsp-bridge-document-highlight-cleanup ()
  "Remove all document highlight overlays from the current buffer."
  (remove-overlays (point-min) (point-max) 'face 'lsp-bridge-document-highlight-face))

(defun lsp-bridge-enable-document-highlight-timer ()
  "Start the repeating document highlight idle timer when it is not set.
The timer runs `lsp-bridge-document-highlight' every half second of
idle time and is stored in `lsp-bridge-document-highlight-timer'."
  (unless lsp-bridge-document-highlight-timer
    (setq lsp-bridge-document-highlight-timer
          (run-with-idle-timer 0.5 t #'lsp-bridge-document-highlight))))

(defun lsp-bridge-toggle-document-highlight ()
  "Toggle document highlight."
  (interactive)
  (if lsp-bridge-enable-document-highlight
      (progn
        (setq lsp-bridge-enable-document-highlight nil)
        (lsp-bridge-document-highlight-cleanup)
        (message "Document highlight disabled"))
    (setq lsp-bridge-enable-document-highlight t)
    (lsp-bridge-enable-document-highlight-timer)
    (lsp-bridge-document-highlight-setup)
    (lsp-bridge-document-highlight)
    (message "Document highlight enabled")))

(defun lsp-bridge-document-highlight-setup ()
  "Add the document highlight cleanup to the local `post-command-hook'."
  (add-hook 'post-command-hook #'lsp-bridge-document-highlight-cleanup nil t))

(provide 'lsp-bridge-workspace)

;;; lsp-bridge-workspace.el ends here
