;;; lsp-bridge-transport.el --- The EPC transport, the process and the protocol handshake -*- lexical-binding: t -*-

;;; Commentary:

;; Part of lsp-bridge, loaded by lsp-bridge.el at the end of its own load.
;; Symbols that live in the other files of the package are declared below rather
;; than required, so that the files never need each other; a macro is autoloaded
;; with the `macro' flag, because a macro that the compiler does not know is
;; compiled as a function call.  Only libraries outside the package are
;; required.  The byte-compile gate reports a symbol that is used and not
;; declared, which keeps these lists honest.

;;; Code:

(require 'json)
(require 'subr-x)

(declare-function lsp-bridge--cleanup "lsp-bridge")
(declare-function lsp-bridge-close-buffer-file "lsp-bridge")
(autoload 'lsp-bridge-deferred-chain "lsp-bridge-epc" nil nil 'macro)
(declare-function lsp-bridge-elisp-symbols-update "lsp-bridge")
(defvar lsp-bridge-enable-debug)
(defvar lsp-bridge-enable-org-babel)
(defvar lsp-bridge-enable-profile)
(declare-function lsp-bridge-epc-call-deferred "lsp-bridge-epc")
(declare-function lsp-bridge-epc-call-sync "lsp-bridge-epc")
(declare-function lsp-bridge-epc-connect "lsp-bridge-epc")
(declare-function lsp-bridge-epc-define-method "lsp-bridge-epc")
(declare-function lsp-bridge-epc-init-epc-layer "lsp-bridge-epc")
(declare-function lsp-bridge-epc-live-p "lsp-bridge-epc")
(defvar lsp-bridge-epc-manager)
(declare-function lsp-bridge-epc-server-start "lsp-bridge-epc")
(declare-function lsp-bridge-epc-stop-epc "lsp-bridge-epc")
(defvar lsp-bridge-get-language-id)
(declare-function lsp-bridge-get-multi-lang-server-by-extension "lsp-bridge")
(declare-function lsp-bridge-get-multi-lang-server-by-file-mode "lsp-bridge")
(defvar lsp-bridge-get-multi-lang-server-by-project)
(defvar lsp-bridge-get-project-path-by-filepath)
(declare-function lsp-bridge-get-single-lang-server-by-extension "lsp-bridge")
(declare-function lsp-bridge-get-single-lang-server-by-file-mode "lsp-bridge")
(defvar lsp-bridge-get-single-lang-server-by-project)
(defvar lsp-bridge-get-workspace-folder)
(declare-function lsp-bridge-has-lsp-server-p "lsp-bridge")
(declare-function lsp-bridge-is-remote-file "lsp-bridge")
(defvar lsp-bridge-log-level)
(defvar lsp-bridge-mode)
(defvar lsp-bridge-org-babel--info-cache)
(defvar lsp-bridge-python-command)
(defvar lsp-bridge-python-file)
(defvar lsp-bridge-remote-save-password)
(declare-function lsp-bridge-remote-send-lsp-request "lsp-bridge-remote")
(declare-function lsp-bridge-search-words-index-files "lsp-bridge")
(defvar lsp-bridge-tsdk-path)

(defvar lsp-bridge-server nil
  "The LSP-Bridge Server.")

(defvar lsp-bridge-server-port nil
  "Port on which the EPC server of the Python side listens.")

(defun lsp-bridge--start-epc-server ()
  "Function to start the EPC server."
  (unless (process-live-p lsp-bridge-server)
    (setq lsp-bridge-server
          (lsp-bridge-epc-server-start
           (lambda (mngr)
             (let ((mngr mngr))
               (lsp-bridge-epc-define-method mngr 'eval-in-emacs 'lsp-bridge--eval-in-emacs-func)
               (lsp-bridge-epc-define-method mngr 'get-emacs-vars 'lsp-bridge--get-emacs-vars-func)
               (lsp-bridge-epc-define-method mngr 'get-project-path 'lsp-bridge--get-project-path-func)
               (lsp-bridge-epc-define-method mngr 'get-language-id 'lsp-bridge--get-language-id-func)
               (lsp-bridge-epc-define-method mngr 'get-workspace-folder 'lsp-bridge--get-workspace-folder-func)
               (lsp-bridge-epc-define-method mngr 'get-multi-lang-server 'lsp-bridge--get-multi-lang-server-func)
               (lsp-bridge-epc-define-method mngr 'get-single-lang-server 'lsp-bridge--get-single-lang-server-func)
               (lsp-bridge-epc-define-method mngr 'get-user-emacs-directory 'lsp-bridge--user-emacs-directory-func)
               (lsp-bridge-epc-define-method mngr 'get-user-tsdk-path 'lsp-bridge--user-tsdk-path-func)
               (lsp-bridge-epc-define-method mngr 'get-buffer-content 'lsp-bridge--get-buffer-content-func)
               (lsp-bridge-epc-define-method mngr 'get-current-line 'lsp-bridge--get-current-line-func)
               (lsp-bridge-epc-define-method mngr 'get-ssh-password 'lsp-bridge--get-ssh-password-func)))))
    (if lsp-bridge-server
        (setq lsp-bridge-server-port (process-contact lsp-bridge-server :service))
      (error "[LSP-Bridge] lsp-bridge-server failed to start")))
  lsp-bridge-server)

(defun lsp-bridge--eval-in-emacs-func (sexp-string)
  "Read and evaluate the elisp form printed in SEXP-STRING.
Always return nil so the EPC reply carries the expected number of
arguments."
  (eval (read sexp-string))
  ;; Return nil to avoid epc error `Got too many arguments in the reply'.
  nil)

(defun lsp-bridge--get-emacs-var-func (var-name)
  "Return the value of the Emacs variable named VAR-NAME.
The result is a list of the value and of a string that says whether
the value is boolean, because the Python side converts nil to []."
  (let* ((var-symbol (intern var-name))
         (var-value (symbol-value var-symbol))
         ;; We need convert result of booleanp to string.
         ;; Otherwise, python-epc will convert all `nil' to [] at Python side.
         (var-is-bool (prin1-to-string (booleanp var-value))))
    (list var-value var-is-bool)))

(defun lsp-bridge--get-emacs-vars-func (&rest vars)
  "Return the values of the Emacs variables named in VARS.
Each element of VARS is a variable name string; see
`lsp-bridge--get-emacs-var-func' for the shape of the result."
  (mapcar #'lsp-bridge--get-emacs-var-func vars))

(defvar lsp-bridge-epc-process nil
  "EPC manager that talks to the lsp_bridge.py process.")

(defvar lsp-bridge-internal-process nil
  "Process object of the running lsp_bridge.py server.")

(defvar lsp-bridge-internal-process-prog nil
  "Program used to start the lsp_bridge.py server.")

(defvar lsp-bridge-internal-process-args nil
  "Argument list used to start the lsp_bridge.py server.")

(defcustom lsp-bridge-name "*lsp-bridge*"
  "Name of LSP-Bridge buffer."
  :type 'string
  :group 'lsp-bridge)

(defun lsp-bridge--get-project-path-func (filename)
  "Return the project root path of FILENAME, search order:

1. Follow the rule of `lsp-bridge-get-project-path-by-filepath'
2. Search up `.dir-locals.el'
3. Search up `.git'"
  (if lsp-bridge-get-project-path-by-filepath
      ;; Fetch project root path by `lsp-bridge-get-project-path-by-filepath' if it set by user.
      (funcall lsp-bridge-get-project-path-by-filepath filename)
    ;; Otherwise try to search up `.dir-locals.el' file
    (let* ((result (dir-locals-find-file filename))
           (dir (if (consp result) (car result) result)))
      (when dir (directory-file-name dir)))))

(defun lsp-bridge--get-language-id-func (project-path file-path server-name extension-name)
  "Return the languageId for FILE-PATH.
PROJECT-PATH, SERVER-NAME and EXTENSION-NAME describe the file and the
server.  `lsp-bridge-get-language-id' wins when it is set, otherwise
the extension of tailwindcss files is mapped to the React language id."
  (if lsp-bridge-get-language-id
      (funcall lsp-bridge-get-language-id project-path file-path server-name extension-name)
    ;; Some LSP server, such as Tailwindcss, languageId is a dynamically field follow with file extension,
    ;; we can't not receive respond to `completionItem/resolve` request if send wrong languageId to tailwindcss.
    ;;
    ;; Please reference issue https://github.com/tailwindlabs/tailwindcss-intellisense/issues/925.
    (when (string-equal server-name "tailwindcss")
      (if (string-equal extension-name "jsx")
          "javascriptreact"
        extension-name))))

(defun lsp-bridge--get-workspace-folder-func (project-path)
  "Return a workspace folder for PROJECT-PATH.
The value comes from `lsp-bridge-get-workspace-folder' and is nil when
that option is unset."
  (when lsp-bridge-get-workspace-folder
    (funcall lsp-bridge-get-workspace-folder project-path)))

(defun lsp-bridge--get-multi-lang-server-func (project-path filename)
  "Return the multi lang server name for FILENAME in PROJECT-PATH.
The project function wins when set, then the extension and the file
mode rules are tried."
  (or (when lsp-bridge-get-multi-lang-server-by-project
        (funcall lsp-bridge-get-multi-lang-server-by-project project-path filename))
      (lsp-bridge-get-multi-lang-server-by-extension filename)
      (lsp-bridge-get-multi-lang-server-by-file-mode filename)))

(defun lsp-bridge--get-single-lang-server-func (project-path filename)
  "Return the single lang server name for FILENAME in PROJECT-PATH.
The project function wins when set, then the extension and the file
mode rules are tried."
  (or (when lsp-bridge-get-single-lang-server-by-project
        (funcall lsp-bridge-get-single-lang-server-by-project project-path filename))
      (lsp-bridge-get-single-lang-server-by-extension filename)
      (lsp-bridge-get-single-lang-server-by-file-mode filename)))

(defun lsp-bridge--user-emacs-directory-func ()
  "Return the user's Emacs directory, expanded.
The Python side asks for it when it has to look at something of this Emacs."
  (expand-file-name user-emacs-directory))

(defun lsp-bridge--user-tsdk-path-func ()
  "Get tsserver lib*.d.ts directory path."
  (when-let* (((null lsp-bridge-tsdk-path))
              (bin (executable-find "tsc"))
              (tsdk (expand-file-name "../../lib" (file-truename bin)))
              ((file-exists-p tsdk)))
    (setq lsp-bridge-tsdk-path tsdk))
  (or lsp-bridge-tsdk-path ""))

(defun lsp-bridge--get-buffer-content-func (buffer-name &optional no-org-babel)
  "Return the content of BUFFER-NAME for the language server.
BUFFER-NAME is the name returned by `buffer-name'.  When
`lsp-bridge-enable-org-babel' is enabled and the buffer is an org
buffer, the current src block content is returned unless
NO-ORG-BABEL is non-nil."
  (when-let* ((buf (get-buffer buffer-name)))
    (if (and lsp-bridge-enable-org-babel
             (eq major-mode 'org-mode) (not no-org-babel))
        (and lsp-bridge-org-babel--info-cache
             (org-element-property :value lsp-bridge-org-babel--info-cache))
      (with-current-buffer buf
        (if (and (eq major-mode 'org-mode) buffer-file-name)
            ;; Org buffer: prefer active C-c ' sub-buffer content.
            ;; When a C-c ' buffer is editing a src block in this org file,
            ;; return the pure src block content (no org markup).
            (let ((c-sub (car (delq nil
                              (mapcar (lambda (b)
                                        (with-current-buffer b
                                          (when (and (bound-and-true-p org-src-mode)
                                                     org-src--beg-marker
                                                     (eq (marker-buffer org-src--beg-marker) buf))
                                            b)))
                                      (buffer-list))))))
              (if c-sub
                  (with-current-buffer c-sub
                    (buffer-substring-no-properties (point-min) (point-max)))
                (buffer-substring-no-properties (point-min) (point-max))))
          (buffer-substring-no-properties (point-min) (point-max)))))))

(defun lsp-bridge--get-current-line-func ()
  "Return the current line of this buffer without text properties."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defvar-local lsp-bridge-tramp-sync-var nil
  "Buffer-local cache used while syncing a tramp file with the server.")

(defun lsp-bridge--get-ssh-password-func (user host port)
  "Return the SSH password of USER on HOST at PORT.
The password is looked up with `auth-source-search', which may prompt
the user; it is saved when `lsp-bridge-remote-save-password' is
non-nil and nil is returned when the user quits the prompt."
  (condition-case nil
      (let* ((auth-source-creation-prompts
              '((secret . "password for %u@%h: ")))
             (found (nth 0 (auth-source-search :max 1
                                               :host host
                                               :user user
                                               :port port
                                               :require '(:secret)
                                               :create t))))
        (when (and lsp-bridge-remote-save-password (plist-get found :save-function))
          (funcall (plist-get found :save-function)))
        (if found
            (auth-info-password found)
          nil))
    (quit (progn
            (message "Cancelled password input.")
            nil))))

(defconst lsp-bridge-protocol-version 2
  "Version of the Emacs <-> Python protocol this file speaks.
The Python side declares the same number, `PROTOCOL_VERSION' in
lsp_bridge.py, and `lsp-bridge--handshake' compares the two when the process
starts.  `scripts/check-protocol.py' refuses to let the two constants drift
apart, and `lsp-bridge-diagnose' reports them.")

(defvar lsp-bridge--server-handshake nil
  "Reply of the protocol handshake, as an alist, or nil before it ran.
Holds the protocol version, the capabilities and the Python version and pid of
the server that answered.")

(defvar lsp-bridge--protocol-error nil
  "Why the handshake with the Python side failed, or nil when it agreed.
Set by `lsp-bridge--handshake' and reported by `lsp-bridge-diagnose'.")

(defun lsp-bridge-call-sync (method &rest args)
  "Call the Python EPC function METHOD with ARGS and return its result.
Blocks until the reply arrives, which is only acceptable while the process is
starting or for a command the user asked for, never on the typing path."
  (lsp-bridge-epc-call-sync lsp-bridge-epc-process (read method) args))

(defun lsp-bridge--handshake ()
  "Exchange protocol versions with the Python side.
Return non-nil when the two halves can talk to each other, and store the reply
in `lsp-bridge--server-handshake'.  When they cannot - the server does not know
the handshake, does not answer, or speaks another protocol - store the reason in
`lsp-bridge--protocol-error' and return nil, so that the caller can leave the
server alone instead of sending it data it cannot understand."
  (setq lsp-bridge--server-handshake nil
        lsp-bridge--protocol-error nil)
  (let (reply failure)
    (condition-case err
        (setq reply (lsp-bridge-call-sync "handshake" lsp-bridge-protocol-version))
      (error (setq failure (format "the server did not answer the handshake (%s)"
                                   (error-message-string err)))))
    (cond
     ;; The call itself failed: an older server, or one that died on us.
     (failure
      (setq lsp-bridge--protocol-error failure)
      nil)
     ;; A reply is only useful when it is the JSON string the server sends.
     ((not (stringp reply))
      (setq lsp-bridge--protocol-error
            (format "the server answered the handshake with %S" reply))
      nil)
     (t
      (let ((handshake (condition-case err
                           (json-parse-string reply :object-type 'alist :array-type 'list)
                         (error (format "the handshake reply is not JSON (%s)"
                                        (error-message-string err))))))
        (cond
         ;; The reply could not be read.
         ((stringp handshake)
          (setq lsp-bridge--protocol-error handshake)
          nil)
         ;; The server answered but speaks another protocol version.
         ((not (eq (alist-get 'compatible handshake) t))
          (setq lsp-bridge--server-handshake handshake
                lsp-bridge--protocol-error
                (format "protocol mismatch: Emacs speaks %s, the server speaks %s"
                        lsp-bridge-protocol-version
                        (alist-get 'protocol_version handshake)))
          nil)
         (t
          (setq lsp-bridge--server-handshake handshake)
          t)))))))

(defun lsp-bridge-call-async (method &rest args)
  "Call Python EPC function METHOD and ARGS asynchronously."
  (lsp-bridge-deferred-chain
    (lsp-bridge-epc-call-deferred lsp-bridge-epc-process (read method) args)))

(defvar-local lsp-bridge-buffer-file-deleted nil
  "Whether the file of this buffer has been deleted on disk.
While it is non-nil lsp-bridge sends no LSP request for the buffer and
clears the flag as soon as the file exists again.")

(defun lsp-bridge-process-live-p ()
  "Return non-nil if the lsp-bridge EPC process is alive."
  (lsp-bridge-epc-live-p lsp-bridge-epc-process))

(defun lsp-bridge-call-file-api-p ()
  "Return non-nil if file LSP requests may be sent from this buffer.
That requires `lsp-bridge-mode', an applicable language server, an
available server command and a live lsp-bridge process."
  (and lsp-bridge-mode
       (lsp-bridge-has-lsp-server-p)
       (if (boundp 'acm-backend-lsp-server-command-exist)
           acm-backend-lsp-server-command-exist
         t)
       (lsp-bridge-process-live-p)))

(defun lsp-bridge-call-file-api (method &rest args)
  "Send the file LSP request METHOD with ARGS for the current buffer.
The request is forwarded to the remote server for remote buffers,
skipped while a tramp file is still being updated, and otherwise sent
with the buffer file path prepended to ARGS.  A `closeFile' request is
sent and the buffer is marked deleted once its file no longer exists."
  (if (lsp-bridge-is-remote-file)
      (lsp-bridge-remote-send-lsp-request method args)
    (if (and buffer-file-name (file-remote-p (buffer-file-name)))
        (message "[LSP-Bridge] remote file \"%s\" is updating info... skip call %s."
                 (buffer-file-name) method)
      (when (lsp-bridge-call-file-api-p)
        (if (and (boundp 'acm-backend-lsp-filepath)
                 (file-exists-p acm-backend-lsp-filepath))
            (if lsp-bridge-buffer-file-deleted
                ;; If buffer's file create again (such as switch branch back), we need save buffer first,
                ;; send the LSP request after the file is changed next time.
                (progn
                  (save-buffer)
                  (setq-local lsp-bridge-buffer-file-deleted nil)
                  (message "[LSP-Bridge] %s is back, will send the %s LSP request after the file is changed next time." acm-backend-lsp-filepath method))
              (when (and acm-backend-lsp-filepath
                         (not (string-equal acm-backend-lsp-filepath "")))
                (lsp-bridge-deferred-chain
                  (lsp-bridge-epc-call-deferred lsp-bridge-epc-process (read method) (append (list acm-backend-lsp-filepath) args)))))
          ;; We need send `closeFile' request to lsp server if we found buffer's file is not exist,
          ;; it is usually caused by switching branch or other tools to delete file.
          ;;
          ;; We won't send any lsp request until buffer's file create again.
          (unless lsp-bridge-buffer-file-deleted
            (lsp-bridge-close-buffer-file)
            (setq-local lsp-bridge-buffer-file-deleted t)
            (message "[LSP-Bridge] %s is not exist, stop send the %s LSP request until file create again." acm-backend-lsp-filepath method)))))))

(defvar lsp-bridge-log-buffer-window nil
  "Window state of the `*lsp-bridge*' buffer before a restart.
Holds a cons of the log buffer window and the window to select again.")

(defun lsp-bridge--process-log-output (process output)
  "Insert PROCESS OUTPUT into log buffer with ANSI text properties."
  (let ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (start (marker-position (process-mark process))))
          (save-excursion
            (goto-char start)
            (insert output)
            (when (or ansi-color-context (string-match-p "\e" output))
              (ansi-color-apply-on-region start (point)))
            (set-marker (process-mark process) (point))))))))

(defun lsp-bridge-restart-process ()
  "Stop and restart LSP-Bridge process."
  (interactive)
  ;; Record log buffer window before restart lsp-bridge process.
  (setq lsp-bridge-log-buffer-window
        (cl-dolist (buffer (buffer-list))
          (when (string-equal (buffer-name buffer) lsp-bridge-name)
            (cl-return (cons (get-buffer-window buffer) (selected-window))))))

  ;; Restart lsp-bridge process.
  (lsp-bridge-kill-process)
  (lsp-bridge-start-process)

  ;; Try restore lsp-bridge log buffer after restart.
  (when lsp-bridge-log-buffer-window
    (save-excursion
      (when (window-live-p (car lsp-bridge-log-buffer-window))
        (select-window (car lsp-bridge-log-buffer-window))
        (switch-to-buffer lsp-bridge-name))
      (select-window (cdr lsp-bridge-log-buffer-window))))

  (message "[LSP-Bridge] Process restarted."))

(defun lsp-bridge-profile-dump ()
  "Ask the server to dump its collected profiling data.
The data is written to ~/lsp-bridge.prof when
`lsp-bridge-enable-profile' is enabled."
  (interactive)
  (lsp-bridge-call-async "profile_dump"))

(defun lsp-bridge-start-process ()
  "Start LSP-Bridge process if it isn't started."
  (if (and (process-live-p lsp-bridge-server)
           (process-live-p lsp-bridge-internal-process))
      (remove-hook 'post-command-hook #'lsp-bridge-start-process)
    ;; start epc server and set `lsp-bridge-server-port'
    (lsp-bridge--start-epc-server)
    (let* ((lsp-bridge-args (append
                             (when (member lsp-bridge-python-command '("pipx" "uv"))
                               (list "run"))
                             (list lsp-bridge-python-file)
                             (list (number-to-string lsp-bridge-server-port))
                             (when lsp-bridge-enable-profile
                               (list "profile")))))

      ;; Set process arguments.
      (if lsp-bridge-enable-debug
          (progn
            (setq lsp-bridge-internal-process-prog "gdb")
            (setq lsp-bridge-internal-process-args (append (list "-batch" "-ex" "run" "-ex" "bt" "--args" lsp-bridge-python-command) lsp-bridge-args)))
        (setq lsp-bridge-internal-process-prog lsp-bridge-python-command)
        (setq lsp-bridge-internal-process-args lsp-bridge-args))

      ;; Start python process.
      (let ((process-connection-type (not (lsp-bridge--called-from-wsl-on-windows-p))))
        (setq lsp-bridge-internal-process
              (apply 'start-process
                     lsp-bridge-name lsp-bridge-name
                     lsp-bridge-internal-process-prog lsp-bridge-internal-process-args)))
      (with-current-buffer (process-buffer lsp-bridge-internal-process)
        (setq-local ansi-color-context nil))
      (set-process-filter lsp-bridge-internal-process #'lsp-bridge--process-log-output)
      (set-process-query-on-exit-flag lsp-bridge-internal-process nil))))

(defun lsp-bridge--called-from-wsl-on-windows-p ()
  "Check whether lsp-bridge is called by Emacs on WSL and is running on Windows."
  (and (eq system-type 'gnu/linux)
       (string-match-p ".exe" lsp-bridge-python-command)))

(defvar lsp-bridge-stop-process-hook nil
  "Hook run before the lsp-bridge server process is killed.
Each function is called with no argument from
`lsp-bridge-kill-process'.")

(defun lsp-bridge-kill-process ()
  "Stop LSP-Bridge process and kill all LSP-Bridge buffers."
  (interactive)

  ;; Clean up.
  (lsp-bridge--cleanup)

  ;; Run stop process hooks.
  (run-hooks 'lsp-bridge-stop-process-hook)

  ;; Kill process after kill buffer, make application can save session data.
  (lsp-bridge--kill-python-process))

(defun lsp-bridge--kill-python-process ()
  "Kill LSP-Bridge background python process."
  (when (lsp-bridge-process-live-p)
    ;; Cleanup before exit LSP-Bridge server process.
    (lsp-bridge-call-async "cleanup")
    ;; Delete LSP-Bridge server process.
    (lsp-bridge-epc-stop-epc lsp-bridge-epc-process)
    ;; Kill *lsp-bridge* buffer.
    (when (get-buffer lsp-bridge-name)
      (kill-buffer lsp-bridge-name))
    (setq lsp-bridge-epc-process nil)
    (setq lsp-bridge-internal-process nil)
    (message "[LSP-Bridge] Process terminated.")))

(defun lsp-bridge-diagnose--report (state)
  "Return the diagnose report of STATE as a string.
STATE is a plist of the facts `lsp-bridge-diagnose' collected: `:elisp-file',
`:protocol-version', `:python-args', `:python-file', `:process', `:handshake',
`:handshake-error', `:server', `:log-buffer', `:log-lines' and `:log-level'.
Keeping the report a pure function of that plist is what makes it testable
without a running server."
  (let* ((handshake (plist-get state :handshake))
         (server (plist-get state :server))
         (capabilities (alist-get 'capabilities handshake)))
    (concat
     "LSP-Bridge diagnose\n"
     "===================\n"
     (format "Emacs side   : %s (protocol %s)\n"
             (or (plist-get state :elisp-file) "not found on `load-path'")
             (plist-get state :protocol-version))
     (format "Python side  : %s\n"
             (mapconcat #'identity (plist-get state :python-args) " "))
     (format "Process      : %s\n"
             (if (plist-get state :process) "live" "not running"))
     (format "Handshake    : %s\n"
             (if (plist-get state :handshake-error)
                 (plist-get state :handshake-error)
               (format "agreed (server protocol %s, python %s, pid %s)"
                       (alist-get 'protocol_version handshake)
                       (alist-get 'python_version handshake)
                       (alist-get 'pid handshake))))
     (format "Capabilities : %s\n"
             (if capabilities
                 (mapconcat #'symbol-name capabilities ", ")
               "unknown"))
     (format "Server facts : %s\n"
             (if server
                 (format "%s language server(s), %s tracked file(s), python %s, pid %s"
                         (alist-get 'lsp_server_count server)
                         (alist-get 'file_action_count server)
                         (alist-get 'python_version server)
                         (alist-get 'pid server))
               "unavailable"))
     (format "Log          : %s (%s lines, level %s)\n"
             (or (plist-get state :log-buffer) "no log buffer")
             (or (plist-get state :log-lines) 0)
             (plist-get state :log-level)))))

(defun lsp-bridge-diagnose ()
  "Report what the Emacs and Python halves of lsp-bridge are doing.
Write into the `*lsp-bridge-diagnose*' buffer the protocol versions of the two
halves and the result of their handshake, whether the process is alive, the
runtime facts the server reports - its Python version, pid, language servers
and tracked files - and where the log goes.  Made for a bug report: the buffer
can be copied as it is."
  (interactive)
  (let* ((server (when (lsp-bridge-process-live-p)
                   (condition-case nil
                       (json-parse-string (lsp-bridge-call-sync "diagnose")
                                          :object-type 'alist :array-type 'list)
                     (error nil))))
         (log-buffer (get-buffer lsp-bridge-name))
         (report (lsp-bridge-diagnose--report
                  (list :elisp-file (locate-library "lsp-bridge")
                        :protocol-version lsp-bridge-protocol-version
                        :python-args (cons lsp-bridge-internal-process-prog
                                           lsp-bridge-internal-process-args)
                        :python-file lsp-bridge-python-file
                        :process (lsp-bridge-process-live-p)
                        :handshake lsp-bridge--server-handshake
                        :handshake-error lsp-bridge--protocol-error
                        :server server
                        :log-buffer (and log-buffer (buffer-name log-buffer))
                        :log-lines (and log-buffer
                                        (with-current-buffer log-buffer
                                          (line-number-at-pos (point-max))))
                        :log-level lsp-bridge-log-level))))
    (with-current-buffer (get-buffer-create "*lsp-bridge-diagnose*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert report))
      (special-mode)
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(defun lsp-bridge--first-start (lsp-bridge-epc-port)
  "Create the EPC manager for the server at LSP-BRIDGE-EPC-PORT.
Then exchanges the protocol handshake and, when the two halves agree on it,
indexes search words and syncs the elisp symbols to the server.  A server that
does not answer the handshake or speaks another protocol is reported and left
alone, so that a mismatched pair fails loudly instead of mysteriously."
  ;; Make EPC process.
  (setq lsp-bridge-epc-process (make-lsp-bridge-epc-manager
                                :server-process lsp-bridge-internal-process
                                :commands (cons lsp-bridge-internal-process-prog lsp-bridge-internal-process-args)
                                :title (mapconcat 'identity (cons lsp-bridge-internal-process-prog lsp-bridge-internal-process-args) " ")
                                :port lsp-bridge-epc-port
                                :connection (lsp-bridge-epc-connect "127.0.0.1" lsp-bridge-epc-port)))
  (lsp-bridge-epc-init-epc-layer lsp-bridge-epc-process)

  (if (lsp-bridge--handshake)
      (progn
        ;; Search words from opened files.
        (lsp-bridge-search-words-index-files)

        ;; Synchronize elisp symbol to Python side.
        (lsp-bridge-elisp-symbols-update))
    (message "[LSP-Bridge] %s; see `lsp-bridge-diagnose'."
             lsp-bridge--protocol-error)))

(provide 'lsp-bridge-transport)

;;; lsp-bridge-transport.el ends here
