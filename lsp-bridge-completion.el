;;; lsp-bridge-completion.el --- Completion requests, candidates and predicates -*- lexical-binding: t -*-

;;; Commentary:

;; Part of lsp-bridge, loaded by lsp-bridge.el at the end of its own load.
;; Symbols that live in the other files of the package are declared below rather
;; than required, so that the files never need each other; a macro is autoloaded
;; with the `macro' flag, because a macro that the compiler does not know is
;; compiled as a function call.  Only libraries outside the package are
;; required.  The byte-compile gate reports a symbol that is used and not
;; declared, which keeps these lists honest.

;;; Code:

(require 'subr-x)
(require 'tramp)

(defvar acm-backend-ctags-max-candidates)
(declare-function acm-backend-jupyter-record "acm-backend-jupyter")
(defvar acm-backend-lsp-fetch-completion-item-ticker)
(defvar acm-backend-path-items)
(declare-function acm-backend-search-file-words-get-point-string "acm-backend-search-file-words")
(declare-function acm-backend-tabby-record "acm-backend-tabby")
(declare-function acm-char-before "acm")
(declare-function acm-doc-hide "acm")
(declare-function acm-doc-try-show "acm")
(defvar acm-enable-copilot)
(defvar acm-enable-ctags)
(defvar acm-enable-jupyter)
(defvar acm-enable-tabby)
(declare-function acm-frame-visible-p "acm-frame")
(declare-function acm-get-input-prefix "acm")
(declare-function acm-hide "acm")
(declare-function acm-in-string-p "acm")
(declare-function acm-in-vulpea-bracket-p "acm")
(defvar acm-input-bound-style)
(autoload 'acm-popup "acm-session" nil nil 'macro)
(declare-function acm-template-candidate-init "acm")
(declare-function acm-update "acm")
(declare-function lsp-bridge--position "lsp-bridge")
(require 'lsp-bridge-core)

;; The macros of `lsp-bridge-core' expand into calls to these, which this
;; file does not define.
(declare-function lsp-bridge--resolve-remote-host "lsp-bridge")
(declare-function lsp-bridge-get-match-buffer-by-filepath "lsp-bridge")
(declare-function lsp-bridge-get-match-buffer-by-remote-file "lsp-bridge")
(declare-function lsp-bridge-call-async "lsp-bridge")
(declare-function lsp-bridge-call-file-api "lsp-bridge")
(defvar lsp-bridge-complete-manually)
(defvar lsp-bridge-completion-hide-characters)
(defvar lsp-bridge-completion-in-string-file-types)
(defvar lsp-bridge-completion-obey-trigger-characters-p)
(defvar lsp-bridge-completion-popup-predicates)
(defvar lsp-bridge-completion-stop-commands)
(declare-function lsp-bridge-elisp-get-filepath "lsp-bridge")
(declare-function lsp-bridge-elisp-symbols-search "lsp-bridge")
(defvar lsp-bridge-enable-completion-in-string)
(defvar lsp-bridge-enable-org-babel)
(declare-function lsp-bridge-get-buffer-file-name-text "lsp-bridge")
(declare-function lsp-bridge-is-org-temp-buffer-p "lsp-bridge")
(declare-function lsp-bridge-is-remote-file "lsp-bridge")
(defvar lsp-bridge-last-change-command)
(defvar lsp-bridge-last-change-is-delete-command-p)
(defvar lsp-bridge-last-change-position)
(defvar lsp-bridge-log-level)
(defvar lsp-bridge-name)
(defvar lsp-bridge-org-babel--update-file-before-change)
(declare-function lsp-bridge-org-babel-send-src-block-to-lsp-server "lsp-bridge-org-babel")
(declare-function lsp-bridge-process-live-p "lsp-bridge")
(defvar lsp-bridge-prohibit-completion)
(defvar lsp-bridge-remote-file-path)
(declare-function lsp-bridge-remote-send-func-request "lsp-bridge-remote")
(defvar lsp-bridge-string-interpolation-open-chars-alist)

(defun lsp-bridge-set-prefix-style (prefix-style)
  "Set the acm input bound style of this buffer to PREFIX-STYLE.
The Wen LSP server needs an ASCII keyword prefix, other servers need
the symbol bounds as keyword prefix."
  ;; Wen LSP server need `acm-get-input-prefix-bound' return ASCII keyword prefix,
  ;; other LSP server need use `bounds-of-thing-at-point' of symbol as keyword prefix.
  (setq-local acm-input-bound-style prefix-style))

(defun lsp-bridge-set-server-names (filename filehost server-names)
  "Store SERVER-NAMES for the buffer visiting FILENAME on FILEHOST.
The buffer is located with `lsp-bridge--with-file-buffer' and the
names are saved in its buffer-local `acm-backend-lsp-server-names'."
  (lsp-bridge--with-file-buffer filename filehost
                                (setq-local acm-backend-lsp-server-names server-names)))

(defun lsp-bridge-completion--record-items (filename
                                            filehost
                                            candidates
                                            position
                                            server-name
                                            completion-trigger-characters
                                            server-names)
  "Record completion CANDIDATES for the buffer of FILENAME and FILEHOST.
POSITION is where the completion started, SERVER-NAME names the server
that answered and COMPLETION-TRIGGER-CHARACTERS and SERVER-NAMES are
stored for later completion requests.  The items are cached in the
buffer and the completion menu is tried immediately."
  ;; Adjust `gc-cons-threshold' to maximize temporary,
  ;; make sure Emacs not do GC
  (let ((gc-cons-threshold most-positive-fixnum))
    (lsp-bridge--with-file-buffer filename filehost
                                  ;; Save completion items.
                                  (setq-local acm-backend-lsp-cache-candidates nil)
                                  (setq-local acm-backend-lsp-completion-position position)
                                  (setq-local acm-backend-lsp-completion-trigger-characters completion-trigger-characters)
                                  (setq-local acm-backend-lsp-server-names server-names)
                                  (setq-local acm-backend-lsp-fetch-completion-item-ticker nil)

                                  (let* ((lsp-items acm-backend-lsp-items)
                                         (completion-table (make-hash-table :test 'equal)))
                                    (dolist (item candidates)
                                      (plist-put item :annotation (capitalize (plist-get item :icon)))
                                      (puthash (plist-get item :key) item completion-table))
                                    (puthash server-name completion-table lsp-items)
                                    (setq-local acm-backend-lsp-items lsp-items))
                                  (lsp-bridge-try-completion))))

(defun lsp-bridge-check-predicate (pred current-function)
  "Return non-nil if PRED allows completion for CURRENT-FUNCTION.
PRED is called with no argument; errors are reported and treated as
non-nil so that a broken predicate does not stop `post-command-hook'.
CURRENT-FUNCTION only names the caller in the error message."
  (if (functionp pred)
      (condition-case err
          (let ((result (funcall pred)))
            (when (eq lsp-bridge-log-level 'debug)
              (unless result
                (with-current-buffer (get-buffer-create lsp-bridge-name)
                  (save-excursion
                    (goto-char (point-max))
                    (insert (format "\n*** %s execute predicate '%s' failed with result: '%s'\n"
                                    current-function pred result))))))
            result)
        (error
         (message "Error in predicate '%s': %S at point %S in buffer %S, mode %S"
                  pred err (point) (current-buffer) major-mode)
         ;; return `t' if got error, avoid break `post-command-hook'
         t))
    t))

(defun lsp-bridge-try-completion ()
  "Popup the completion menu when the buffer state allows it.
The menu is shown only when the buffer and cursor are unchanged since
the completion request and every predicate in
`lsp-bridge-completion-popup-predicates' returns non-nil; when a
predicate refuses, the menu is hidden instead."
  (cond (lsp-bridge-prohibit-completion
         (setq-local lsp-bridge-prohibit-completion nil))
        (t
         ;; Don't popup completion menu when `lsp-bridge-last-change-position' (cursor before send completion request) is not equal current cursor position.
         (when (equal lsp-bridge-last-change-position
                      (list (current-buffer) (buffer-chars-modified-tick) (point)))
           ;; Try popup completion frame.
           (if (cl-every (lambda (pred)
                           (lsp-bridge-check-predicate pred "lsp-bridge-try-completion"))
                         lsp-bridge-completion-popup-predicates)
               (progn
                 (acm-template-candidate-init)
                 (acm-update)

                 ;; We need reset `lsp-bridge-manual-complete-flag' if completion menu popup by `lsp-bridge-popup-complete-menu'.
                 (when lsp-bridge-complete-manually
                   (setq-local lsp-bridge-manual-complete-flag nil)))
             (acm-hide))))))

(defun lsp-bridge-record-last-change-position ()
  "Record the current buffer, modification tick and cursor position.
The record is stored in `lsp-bridge-last-change-position' and lets
`lsp-bridge-try-completion' detect buffer changes made since a
completion request was sent."
  (setq lsp-bridge-last-change-position
        (list (current-buffer) (buffer-chars-modified-tick) (point))))

(defun lsp-bridge-popup-complete-menu ()
  "Popup the completion menu on explicit request.
Sets `lsp-bridge-manual-complete-flag', records the current position,
syncs an org src block when Babel is enabled and sends a
`try_completion' request to the server."
  (interactive)
  ;; Set `lsp-bridge-manual-complete-flag' to non-nil, make sure poup completion menu once.
  (setq-local lsp-bridge-manual-complete-flag t)

  ;; Record last change position make sure `lsp-bridge-try-completion' will popup completion menu.
  (lsp-bridge-record-last-change-position)

  ;; Sync src block content to lsp server make sure lsp-bridge can popup completion menu even user don't change code in src block.
  (when (and lsp-bridge-enable-org-babel (eq major-mode 'org-mode))
    (setq-local lsp-bridge-org-babel--update-file-before-change t)
    (lsp-bridge-org-babel-send-src-block-to-lsp-server))

  ;; We send `try_completion' request directly, because user input nothing before call command `lsp-bridge-popup-complete-menu'.
  (lsp-bridge-call-file-api "try_completion"
                            (lsp-bridge--position)
                            (acm-char-before)
                            (acm-get-input-prefix))

  ;; Complete other non-LSP backends.
  (lsp-bridge-complete-other-backends))

(defun lsp-bridge--not-match-stop-commands ()
  "Return non-nil if the last command is not a completion stop command.
`lsp-bridge-last-change-command' and `last-command' are both checked,
as command names, against `lsp-bridge-completion-stop-commands'."
  (not (or (member (format "%s" lsp-bridge-last-change-command)
                   lsp-bridge-completion-stop-commands)
           (member (lsp-bridge-command-name last-command) lsp-bridge-completion-stop-commands))))

(defun lsp-bridge-string-interpolation-p (string-interpolation-open-chars-alist)
  "Return non-nil if point is inside a string interpolation.
STRING-INTERPOLATION-OPEN-CHARS-ALIST maps the major mode to the
regexp that opens an interpolation."
  (when-let* ((search-char (cdr (assoc (buffer-local-value 'major-mode (current-buffer))
                                       string-interpolation-open-chars-alist)))
              (open-pos (save-excursion (search-backward-regexp search-char nil t))))
    (not (save-excursion (search-backward-regexp "\}" open-pos t)))))

(defun lsp-bridge--not-in-string ()
  "Hide completion if cursor in string area."
  (or
   ;; Sources that know better than the syntax table, such as one that completes
   ;; a code fence language inside what the table calls a string.
   (run-hook-with-args-until-success 'lsp-bridge-completion-in-string-functions)
   ;; Allow completion in string.
   lsp-bridge-enable-completion-in-string
   ;; For some languages, allow popup completion menu in string.
   (and (boundp 'acm-backend-lsp-filepath)
        acm-backend-lsp-filepath
        (member (file-name-extension acm-backend-lsp-filepath) lsp-bridge-completion-in-string-file-types))
   ;; Other language not allowed popup completion in string, it's annoy
   (not (acm-in-string-p))
   ;; Allow popup completion menu for string interpolation
   (lsp-bridge-string-interpolation-p lsp-bridge-string-interpolation-open-chars-alist)
   ;; Allow file path completion in string area
   (ignore-errors
     (and (lsp-bridge-elisp-get-filepath)
          (or (file-exists-p (file-name-directory (lsp-bridge-elisp-get-filepath)))
              ;; Allow string in lsp-bridge-remote file.
              (lsp-bridge-is-remote-file))))))

(defun lsp-bridge--not-execute-macro ()
  "Hide completion during executing macros."
  (not executing-kbd-macro))

(defun lsp-bridge--not-in-mark-macro ()
  "Hide completion markmacro enable."
  (not (and (featurep 'markmacro)
            markmacro-overlays)))

(defun lsp-bridge--not-delete-command ()
  "Hide completion menu if last command is delete command."
  (not lsp-bridge-last-change-is-delete-command-p))

(defun lsp-bridge--not-follow-complete ()
  "Hide completion if last command is `acm-complete'."
  (or
   (not (member (lsp-bridge-command-name last-command) '("acm-complete" "acm-complete-quick-access")))
   (member (lsp-bridge-command-name) '("self-insert-command" "org-self-insert-command"))))

(defun lsp-bridge--not-only-blank-before-cursor ()
  "Hide completion if only blank before cursor, except in a note bracket."
  (or (not
       (null
        (split-string (buffer-substring-no-properties
                       (max (1- (point)) (line-beginning-position))
                       (point)))))
      ;; Keep completion after a space inside a Vulpea link bracket.
      (acm-in-vulpea-bracket-p)))

(defvar lsp-bridge-completion-hide-character-functions nil
  "Functions that may keep the menu from being hidden.
Each is called with no arguments in the buffer completion was asked in and
should return non-nil when the character before point must not hide the menu -
a chat buffer where a colon starts a keyword, for instance.  This is the
extension point for that question, rather than advice on
`lsp-bridge--not-match-hide-characters', which is this package's own function.")

(defvar lsp-bridge-completion-in-string-functions nil
  "Functions that may allow the menu inside a string.
Each is called with no arguments and should return non-nil when completion has
to work inside a string anyway - a source that completes a code fence language,
for instance.  This is the extension point for that question, rather than
advice on `lsp-bridge--not-in-string', which is this package's own function.")

(defun lsp-bridge--not-match-hide-characters ()
  "Return non-nil if the character before point allows completion.
A character listed in `lsp-bridge-completion-hide-characters' hides the menu
unless it is a trigger character of the language server, or a function in
`lsp-bridge-completion-hide-character-functions' says otherwise."
  (let ((char (ignore-errors (char-to-string (char-before)))))
    (or (run-hook-with-args-until-success 'lsp-bridge-completion-hide-character-functions)
        (and lsp-bridge-completion-obey-trigger-characters-p
             (member char (if (boundp 'acm-backend-lsp-completion-trigger-characters)
                              (symbol-value 'acm-backend-lsp-completion-trigger-characters))))
        (not (member char lsp-bridge-completion-hide-characters)))))

(defun lsp-bridge--not-match-hide-keywords ()
  "Hide completion if string before cursor match some special keywords."
  (let ((string (if (bounds-of-thing-at-point 'symbol)
                    (buffer-substring-no-properties (car (bounds-of-thing-at-point 'symbol))
                                                    (point))
                  (char-to-string (char-before)))))
    (not (when (and (or (derived-mode-p 'ruby-mode)
                        (derived-mode-p 'ruby-ts-mode)
                        (derived-mode-p 'elixir-mode)
                        (derived-mode-p 'elixir-ts-mode))
                    (member string '("do" "end")))))))

(defun lsp-bridge--is-evil-state ()
  "If `evil' mode is enable, only show completion when evil is in insert mode."
  (or (not (featurep 'evil))
      (eq (evil-insert-state-p) evil-mode)
      (evil-emacs-state-p)))

(defun lsp-bridge--is-meow-state ()
  "If `meow' mode is enable, only show completion when meow is in insert mode."
  (or (not (featurep 'meow))
      (eq meow-insert-mode meow-mode)
      (minibufferp)))

(defun lsp-bridge--not-in-multiple-cursors ()
  "If `multiple-cursors' mode is enable, hide completion menu."
  (not (and (featurep 'multiple-cursors)
            multiple-cursors-mode)))

(defun lsp-brige--not-in-chatgpt-response ()
  "Don't popup completion menu if ChatGPT is responsing."
  (not (and (boundp 'mind-wave-is-response-p)
            mind-wave-is-response-p)))

(defvar-local lsp-bridge-manual-complete-flag nil
  "Non-nil means the next completion pops up on user request.
Set by `lsp-bridge-popup-complete-menu' and reset once the menu is
shown.")

(defun lsp-bridge--not-complete-manually ()
  "If `lsp-bridge-complete-manually' is non-nil, hide completion menu."
  (or
   ;; Don't hide completion menu if it has show up.
   (acm-frame-visible-p (acm-popup menu-frame))

   ;; Show completion menu when `lsp-bridge-complete-manually' and `lsp-bridge-manual-complete-flag' are non-nil.
   ;; If `lsp-bridge-complete-manually' is nil, not check `lsp-bridge-manual-complete-flag'.
   (not lsp-bridge-complete-manually)
   lsp-bridge-manual-complete-flag))

(defun lsp-bridge--not-in-org-table ()
  "Return non-nil if point is not inside an org table."
  (not (and (boundp 'org-at-table-p)
            (org-at-table-p))))

(defun lsp-bridge-complete-other-backends ()
  "Trigger the non-LSP completion backends.
Copilot, jupyter, tabby and ctags are each asked for candidates when
their acm option is enabled and the current command and buffer allow
it."
  (let* ((this-command-string (lsp-bridge-command-name))
         (current-word (thing-at-point 'word t))
         (current-symbol (thing-at-point 'symbol t)))
    ;; Copilot search.
    (when (and acm-enable-copilot
               (lsp-bridge-process-live-p)
               ;; Copilot backend not support remote file now, disable it temporary.
               (not (lsp-bridge-is-remote-file))
               ;; Don't enable copilot on Markdown mode, Org mode, ielm and minibuffer, very disruptive to writing.
               (not (or (derived-mode-p 'markdown-mode)
                        (eq major-mode 'org-mode)
                        (derived-mode-p 'inferior-emacs-lisp-mode)
                        (minibufferp))))
      (lsp-bridge-copilot-complete))

    ;; emacs jupyter
    (when (and acm-enable-jupyter
               (lsp-bridge-process-live-p)
               (and lsp-bridge-enable-org-babel
                    (eq major-mode 'org-mode)
                    (not (lsp-bridge-is-org-temp-buffer-p)))
               (org-in-src-block-p 'inside)
               (string-prefix-p "jupyter" (plist-get (car (cdr (org-element-context))) :language)))
      (acm-backend-jupyter-record current-symbol))

    ;; tabby complete
    (when (and acm-enable-tabby
               (lsp-bridge-process-live-p))
      (acm-backend-tabby-record current-symbol))

    (when (and acm-enable-ctags
               (lsp-bridge-process-live-p))
      (unless (or (string-equal current-word "") (null current-word))
        (if (lsp-bridge-is-remote-file)
            ;; remote file buffer do not associate with an actual file on the disk
            ;; the buffer is created by lsp-bridge-opne-remote-file--response
            ;; hence (buffer-file-name) will return nil
            ;; should use buffer local variable lsp-bridge-remote-file-path
            (with-current-buffer (buffer-name)
              (lsp-bridge-remote-send-func-request "ctags_complete"
                                                   (list
                                                    current-word
                                                    (tramp-file-local-name lsp-bridge-remote-file-path)
                                                    acm-backend-ctags-max-candidates
                                                    (1- (point)))))
          (lsp-bridge-call-async "ctags_complete" current-word (buffer-file-name) acm-backend-ctags-max-candidates (1- (point))))))

    ;; Search elisp symbol.
    (lsp-bridge-elisp-symbols-search current-symbol)

    ;; Send change file to search-words backend.
    (unless lsp-bridge-prohibit-completion
      (when (or buffer-file-name
                (lsp-bridge-is-remote-file))
        (let ((current-word (acm-backend-search-file-words-get-point-string)))
          ;; Search words if current prefix is not empty.
          (unless (or (string-equal current-word "") (null current-word))
            (if (lsp-bridge-is-remote-file)
                (lsp-bridge-remote-send-func-request "search_file_words_search" (list current-word))
              (lsp-bridge-call-async "search_file_words_search" current-word))))))

    ;; Send path search request when detect path string.
    (if (acm-in-string-p)
        (when-let* ((filename (lsp-bridge-elisp-get-filepath))
                    (dirname (ignore-errors (expand-file-name (file-name-directory filename)))))
          (if (lsp-bridge-is-remote-file)
              (let ((path (if (tramp-tramp-file-p dirname)
                              (tramp-file-name-localname (tramp-dissect-file-name dirname))
                            dirname)))
                (lsp-bridge-remote-send-func-request "search_paths_search"
                                                     (list path (file-name-base filename))))

            (when (file-exists-p dirname)
              (lsp-bridge-call-async "search_paths_search"
                                     dirname
                                     (file-name-base filename)))))
      ;; We need cleanup `acm-backend-path-items' when cursor not in string.
      ;; Otherwise, other completion backend won't show up.
      (setq-local acm-backend-path-items nil))))

(defun lsp-bridge-completion-ui-visible-p ()
  "Return non-nil if the acm completion menu frame is visible."
  (acm-frame-visible-p (acm-popup menu-frame)))

(defun lsp-bridge-completion-item--update (info filehost)
  "Apply the resolved completion item INFO for FILEHOST.
INFO carries the file path, item key, server name, documentation and
additional text edits of the item; the cached item of the buffer is
updated and the documentation frame is shown or hidden accordingly."
  (let* ((filename (plist-get info :filepath))
         (key (plist-get info :key))
         (server-name (plist-get info :server))
         (additional-text-edits (plist-get info :additionalTextEdits))
         (documentation (plist-get info :documentation))
         (has-doc-p (and documentation
                         (not (string-equal documentation "")))))
    (lsp-bridge--with-file-buffer
        filename filehost
        ;; When lsp-bridge running in server, `acm-backend-lsp-items' maybe nil when receive `lsp-bridge-completion-item--update' response.
        ;; So we need check `acm-backend-lsp-items' value before update item documentation.
        (when-let* ((server-lsp-items (gethash server-name acm-backend-lsp-items)))
          ;; Update `documentation' and `additionalTextEdits'
          (when-let* ((item (gethash key server-lsp-items)))
            (when additional-text-edits
              (plist-put item :additionalTextEdits additional-text-edits))

            (when has-doc-p
              (plist-put item :documentation documentation))

            (puthash key item (gethash server-name acm-backend-lsp-items)))

          (if has-doc-p
              ;; Show doc frame if `documentation' exist and not empty.
              (acm-doc-try-show t)
            ;; Hide doc frame immediately.
            (acm-doc-hide))))))

(defun lsp-bridge--not-acm-doc-markdown-buffer ()
  "Return non-nil when the current buffer is not an acm doc temp buffer.
The `markdown-code-fontification:' buffers created while rendering acm
documentation are excluded from the change monitors."
  (not (string-prefix-p " markdown-code-fontification:" (buffer-name (current-buffer)))))

(defun lsp-bridge--not-mind-wave-chat-buffer ()
  "Not mind-wave's `*.chat' buffer."
  (or
   (not (buffer-file-name))
   (not (string-equal (file-name-extension (buffer-file-name)) "chat"))))

(defun lsp-bridge--not-zsh-buffer ()
  "Check if the file extension matches .zsh, .zshenv, or .zshrc."
  (not (ignore-errors
         (or (string-equal (file-name-extension (buffer-file-name)) "zsh")
             (member (buffer-name) '(".zshenv" ".zshrc"))))))

(defun lsp-bridge-copilot-complete ()
  "Ask the copilot backend to complete at point.
The whole buffer text, the major mode, the file path and the relative
path are sent to the server, which returns the completion candidates."
  (interactive)
  (setq-local acm-backend-lsp-fetch-completion-item-ticker nil)
  (let ((all-text (buffer-substring-no-properties (point-min) (point-max)))
        (relative-path
         ;; from copilot.el
         (cond
          ((not buffer-file-name)
           "")
          ((fboundp 'projectile-project-root)
           (file-relative-name
            (lsp-bridge-get-buffer-file-name-text)
            (projectile-project-root)))
          ((boundp 'vc-root-dir)
           (file-relative-name
            (lsp-bridge-get-buffer-file-name-text)
            (vc-root-dir)))
          (t
           (file-name-nondirectory buffer-file-name)))))
    (if (lsp-bridge-is-remote-file)
        (lsp-bridge-remote-send-func-request "copilot_complete"
                                             (list
                                              (lsp-bridge--position)
                                              (symbol-name major-mode)
                                              (buffer-file-name)
                                              relative-path
                                              tab-width
                                              all-text
                                              (not indent-tabs-mode)))
      (lsp-bridge-call-async "copilot_complete"
                             (lsp-bridge--position)
                             (symbol-name major-mode)
                             (buffer-file-name)
                             relative-path
                             tab-width
                             all-text
                             (not indent-tabs-mode)))))

(defun lsp-bridge-search-backend--record-items (backend-name items)
  "Store ITEMS as the candidates of BACKEND-NAME.
Both the candidates and their cache are saved in buffer-local
variables named after BACKEND-NAME and the completion menu is tried."
  ;; Adjust `gc-cons-threshold' to maximize temporary,
  ;; make sure Emacs not do GC
  (let ((gc-cons-threshold most-positive-fixnum))
    (set (make-local-variable (intern (format "acm-backend-%s-items" backend-name))) items)
    (set (make-local-variable (intern (format "acm-backend-%s-cache-candiates" backend-name))) nil)

    (lsp-bridge-try-completion)))

(provide 'lsp-bridge-completion)

;;; lsp-bridge-completion.el ends here
