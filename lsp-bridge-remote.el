;;; lsp-bridge-remote.el --- Remote file and tramp support -*- lexical-binding: t -*-

;;; Commentary:

;; Part of lsp-bridge, loaded by lsp-bridge.el at the end of its own load.
;; Symbols that live in the other files of the package are declared below rather
;; than required, so that the files never need each other; the libraries this
;; file uses on its own are required, because a macro that is not required is
;; silently compiled as a function call.  The byte-compile gate reports a symbol
;; that is used and not declared, which is what keeps these lists honest.

;;; Code:

(require 'cl-lib)
(require 'tramp)
(require 'subr-x)

(declare-function lsp-bridge--record-mark-ring "lsp-bridge")
(declare-function lsp-bridge--set-mark-ring-in-new-buffer "lsp-bridge")
(declare-function lsp-bridge--user-emacs-directory-func "lsp-bridge")
(require 'lsp-bridge-core)

;; The macros of `lsp-bridge-core' expand into calls to these, which this
;; file does not define.
(declare-function lsp-bridge--resolve-remote-host "lsp-bridge")
(declare-function lsp-bridge-get-match-buffer-by-filepath "lsp-bridge")
(declare-function lsp-bridge-get-match-buffer-by-remote-file "lsp-bridge")
(declare-function lsp-bridge-call-async "lsp-bridge")
(autoload 'lsp-bridge-deferred-chain "lsp-bridge-epc" nil nil 'macro)
(declare-function lsp-bridge-define--jump-flash "lsp-bridge")
(declare-function lsp-bridge-epc-call-deferred "lsp-bridge-epc")
(defvar lsp-bridge-epc-process)
(declare-function lsp-bridge-get-buffer-file-name-text "lsp-bridge")
(declare-function lsp-bridge-get-mode-name-from-file-path "lsp-bridge")
(declare-function lsp-bridge-mode "lsp-bridge")
(defvar lsp-bridge-mode)
(defvar lsp-bridge-ref-open-remote-file-go-back-to-ref-window)
(defvar lsp-bridge-remote-start-automatically)
(declare-function lsp-bridge-switch-to-ref-window "lsp-bridge-ref")
(defvar lsp-bridge-tramp-alias-alist)
(defvar lsp-bridge-tramp-blacklist)
(defvar lsp-bridge-tramp-connection-info)
(defvar lsp-bridge-tramp-sync-var)

(defvar-local lsp-bridge-remote-file-flag nil
  "Non-nil means this buffer was opened for a remote file.")

(defvar-local lsp-bridge-remote-file-tramp-method nil
  "Tramp method of the remote file of this buffer.")

(defvar-local lsp-bridge-remote-file-user nil
  "Remote user of the remote file of this buffer.")

(defvar-local lsp-bridge-remote-file-host nil
  "Remote host of the remote file of this buffer.")

(defvar-local lsp-bridge-remote-file-port nil
  "Remote port of the remote file of this buffer.")

(defvar-local lsp-bridge-remote-file-path nil
  "Path of the remote file of this buffer on the remote host.")
;; TODO need to consider tramp docker file pattern
(defvar lsp-bridge-remote-file-pattern
  (rx bos (? "/")
      ;; username
      (? (seq (any "a-z_") (* (any "a-z0-9_.-"))) "@")
      ;; host ip
      (repeat 3 (seq (repeat 1 3 (any "0-9")) ".")) (repeat 1 3 (any "0-9"))
      ;; ssh port
      (? ":" (+ digit))
      ;; path
      (? ":") (? "~") (* nonl) eol)
  "Regexp matching the `host:path' text of a remote file request.
Used to detect that a minibuffer input names a remote file; the
TODO notes that tramp docker patterns are not covered yet.")

(defun lsp-bridge-open-or-create-file (filepath)
  "Open FILEPATH, if it exists.  If not, create it and its parents."
  (if (file-exists-p filepath)
      (with-temp-buffer
        (insert-file-contents filepath)
        (buffer-string))
    (make-directory (file-name-directory filepath) t)
    (with-temp-file filepath)
    ""))

;;;###autoload
(defun lsp-bridge-open-remote-file ()
  "Ask for a remote `ip:path' file and open it on the server.
Recent entries are offered from the ip.txt file in the user Emacs
directory, which is created when missing."
  (interactive)
  (let* ((ip-file (concat (lsp-bridge--user-emacs-directory-func)
                          (file-name-as-directory "lsp-bridge")
                          (file-name-as-directory "remote_file")
                          "ip.txt"))
         (path (completing-read "Open remote file (ip:path): "
                                (with-temp-buffer
                                  (insert (lsp-bridge-open-or-create-file ip-file))
                                  (split-string (buffer-string) "\n" t)))))
    (lsp-bridge-call-async "open_remote_file" path (list :line 0 :character 0))))

(cl-defmacro lsp-bridge--conditional-update-tramp-file-info (tramp-file-name path host &rest body)
  "Conditionally execute BODY with the buffer associated with TRAMP-FILE-NAME.

If the buffer is created by TRAMP with TRAMP-FILE-NAME, BODY is executed within
the context of that buffer.

If the buffer is created by `lsp-bridge-open-remote-file--response',
`lsp-bridge--with-file-buffer' is used with PATH and HOST to find the buffer,
then BODY is executed within that buffer."

  `(if (get-file-buffer ,tramp-file-name)
       (with-current-buffer (get-file-buffer ,tramp-file-name)
         ,@body)
     ;; lsp-bridge--with-file-buffer is another macro, carefully expand it
     ,(macroexpand `(lsp-bridge--with-file-buffer ,path ,host ,@body))))

(defun lsp-bridge-update-tramp-file-info (tramp-file-name tramp-connection-info tramp-method user host path)
  "Store the tramp connection info of TRAMP-FILE-NAME and its buffer.
TRAMP-CONNECTION-INFO, TRAMP-METHOD, USER, HOST and PATH describe the
connection and the file; both alias alists are updated and the buffer
of the file is marked as a remote file."
  (unless (assoc host lsp-bridge-tramp-alias-alist)
    (push `(,host . ,tramp-connection-info) lsp-bridge-tramp-alias-alist))

  (unless (assoc tramp-connection-info lsp-bridge-tramp-connection-info)
    (push `(,tramp-connection-info . ,host) lsp-bridge-tramp-connection-info))

  (lsp-bridge--conditional-update-tramp-file-info tramp-file-name path host
                                                  (setq-local lsp-bridge-remote-file-flag t)
                                                  (setq-local lsp-bridge-remote-file-tramp-method tramp-method)
                                                  (setq-local lsp-bridge-remote-file-user user)
                                                  (setq-local lsp-bridge-remote-file-host host)
                                                  (setq-local lsp-bridge-remote-file-path path)

                                                  (read-only-mode -1)

                                                  (add-hook 'kill-buffer-hook 'lsp-bridge-remote-kill-buffer nil t)
                                                  (setq lsp-bridge-tramp-sync-var t)
                                                  (message "[LSP-Bridge] remote file %s updated info successfully."
                                                           (buffer-file-name))))

;;;###autoload
(defun lsp-bridge-tramp-show-hostnames ()
  "Ask the server to message the host names it knows about."
  (interactive)
  (lsp-bridge-call-async "message_hostnames"))


;;;###autoload
(defun lsp-bridge-sync-tramp-remote (force)
  "Sync the tramp file of the current buffer with the remote server.
FORCE non-nil opens a fresh connection, otherwise a cached connection
is reused when one exists; sudo and blacklisted hosts are skipped."
  (interactive)
  (let* ((file-name (lsp-bridge-get-buffer-file-name-text))
         (tramp-vec (tramp-dissect-file-name file-name))
         (tramp-method (tramp-file-name-method tramp-vec))
         (user (tramp-file-name-user tramp-vec))
         (host (tramp-file-name-host tramp-vec))
         (port (tramp-file-name-port tramp-vec))
         (path (tramp-file-name-localname tramp-vec))
         (tramp-connection-info (substring file-name 0 (+ 1 (string-match ":" file-name (+ 1 (string-match ":" file-name))))))
         (connected-host (cdr (assoc tramp-connection-info lsp-bridge-tramp-connection-info))))

    (if (or force (not connected-host))
        (when (and (not (member tramp-method '("sudo" "sudoedit" "su" "doas")))
                   (not (member host lsp-bridge-tramp-blacklist)))
          (read-only-mode 1)
          (lsp-bridge-call-async "sync_tramp_remote" file-name tramp-method user host port path))

      (lsp-bridge--conditional-update-tramp-file-info file-name path connected-host
                                                      (setq-local lsp-bridge-remote-file-flag t)
                                                      (setq-local lsp-bridge-remote-file-tramp-method tramp-method)
                                                      (setq-local lsp-bridge-remote-file-user user)
                                                      (setq-local lsp-bridge-remote-file-host connected-host)
                                                      (setq-local lsp-bridge-remote-file-path path)

                                                      (add-hook 'kill-buffer-hook 'lsp-bridge-remote-kill-buffer nil t)
                                                      (setq lsp-bridge-tramp-sync-var t)))))

(defun lsp-bridge-get-match-buffer-by-filehost (remote-file-host)
  "Return the first buffer whose remote file host is REMOTE-FILE-HOST."
  (cl-dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when-let* ((match-buffer (string-equal lsp-bridge-remote-file-host remote-file-host)))
        (cl-return buffer)))))

(defun lsp-bridge-construct-tramp-file-name (method user host port path)
  "Construct a TRAMP file name from components: METHOD USER HOST PORT PATH.

SSH tramp file name is like /ssh:user@host#port:path"
  (concat "/"
          method ":"
          (when user (concat user "@"))
          host
          (when port (concat "#" port))
          ":" path))

(defun lsp-bridge-remote-reconnect (remote-file-host force)
  "Reconnect the buffer of REMOTE-FILE-HOST to its tramp connection.
FORCE is passed to `lsp-bridge-sync-tramp-remote'; the call only runs
when `lsp-bridge-remote-start-automatically' is non-nil."
  (when lsp-bridge-remote-start-automatically
    (with-current-buffer (lsp-bridge-get-match-buffer-by-filehost remote-file-host)
      ;; unify handling logic for buffer created by `lsp-bridge-open-remote-file' or open by tramp
      ;; override the buffer-file-name used by `lsp-bridge-sync-tramp-remote'
      ;; if buffer is created by lsp-bridge-open-remote-file then (buffer-file-name) returns nil
      (let ((buffer-file-name (or (buffer-file-name)
                                  (lsp-bridge-construct-tramp-file-name lsp-bridge-remote-file-tramp-method
                                                                        lsp-bridge-remote-file-user
                                                                        lsp-bridge-remote-file-host
                                                                        lsp-bridge-remote-file-port
                                                                        lsp-bridge-remote-file-path))))
        (lsp-bridge-sync-tramp-remote force)))))

(defvar lsp-bridge-remote-file-window nil
  "Window to select again after a remote file response was handled.")

(defun lsp-bridge-open-remote-file--response (tramp-method user host port path content position)
  "Show the remote file content received from the server.
TRAMP-METHOD, USER, HOST, PORT and PATH describe the connection and
the file, CONTENT is the base64 encoded file body and POSITION where
to move point.  The `[LBR]' buffer is created or reused, marked as a
remote file and lsp-bridge is enabled in it."
  (if lsp-bridge-remote-file-window
      (progn
        (select-window lsp-bridge-remote-file-window)
        (setq lsp-bridge-remote-file-window nil)))
  (let ((buf-name (format "[LBR] %s" (file-name-nondirectory path))))
    (lsp-bridge--record-mark-ring)

    (with-current-buffer (get-buffer-create buf-name)
      (text-mode)
      (read-only-mode -1)
      (erase-buffer)
      (insert (lsp-bridge-decode-base64 content))
      (goto-char (point-min))

      (add-hook 'kill-buffer-hook 'lsp-bridge-remote-kill-buffer nil t)

      (let ((mode (lsp-bridge-get-mode-name-from-file-path path)))
        (when mode
          (let ((lsp-bridge-remote-file-flag t)
                (lsp-bridge-remote-file-tramp-method tramp-method)
                (lsp-bridge-remote-file-user user)
                (lsp-bridge-remote-file-host host)
                (lsp-bridge-remote-file-port port)
                (lsp-bridge-remote-file-path path))
            (funcall mode)))))

    (switch-to-buffer buf-name)

    (unless (equal position (list :line 0 :character 0))
      (lsp-bridge--set-mark-ring-in-new-buffer)

      (lsp-bridge-define--jump-flash position))

    (setq-local lsp-bridge-remote-file-flag t)
    (setq-local lsp-bridge-remote-file-tramp-method tramp-method)
    (setq-local lsp-bridge-remote-file-user user)
    (setq-local lsp-bridge-remote-file-host host)
    (setq-local lsp-bridge-remote-file-port port)
    (setq-local lsp-bridge-remote-file-path path)

    ;; Always enable lsp-bridge for remote file.
    ;; Remote file can always edit and update content even some file haven't corresponding lsp server, such as *.txt
    (lsp-bridge-mode 1))

  (when lsp-bridge-ref-open-remote-file-go-back-to-ref-window
    (lsp-bridge-switch-to-ref-window)
    (setq lsp-bridge-ref-open-remote-file-go-back-to-ref-window nil)))

(defun lsp-bridge-remote-kill-buffer ()
  "Tell the server that a remote file buffer was killed.
Runs in `kill-buffer-hook' of remote buffers and sends a
`close_remote_file' request with the host and path."
  (when lsp-bridge-remote-file-flag
    (lsp-bridge-call-async "close_remote_file" lsp-bridge-remote-file-host lsp-bridge-remote-file-path)))

(defun lsp-bridge-decode-base64 (base64-string)
  "Return BASE64-STRING decoded as UTF-8 text."
  (decode-coding-string (base64-decode-string base64-string) 'utf-8))

(defun lsp-bridge-remote-send-lsp-request (method &rest args)
  "Send the LSP request METHOD with ARGS for the current remote file.
The remote file host and path of the buffer are sent along with the
request."
  (lsp-bridge-deferred-chain
    (lsp-bridge-epc-call-deferred lsp-bridge-epc-process
                                  (read "lsp_request")
                                  (append
                                   (list lsp-bridge-remote-file-host lsp-bridge-remote-file-path method) args))))

(defun lsp-bridge-remote-send-func-request (method &rest args)
  "Send the lsp-bridge function METHOD with ARGS for the remote file.
The remote file host and path of the buffer are sent along with the
request."
  (lsp-bridge-deferred-chain
    (lsp-bridge-epc-call-deferred lsp-bridge-epc-process
                                  (read "func_request")
                                  (append
                                   (list lsp-bridge-remote-file-host lsp-bridge-remote-file-path method) args))))

;;;###autoload
(defun lsp-bridge-remote-save-buffer ()
  "Save the remote file of this buffer on the remote host.
A message is shown when the current buffer is not a remote file."
  (interactive)
  (if lsp-bridge-remote-file-flag
      (lsp-bridge-call-async "save_remote_file" lsp-bridge-remote-file-host lsp-bridge-remote-file-path)
    (message "lsp-bridge-remote-save-buffer only for lsp-bridge-remote file.")))

(provide 'lsp-bridge-remote)

;;; lsp-bridge-remote.el ends here
