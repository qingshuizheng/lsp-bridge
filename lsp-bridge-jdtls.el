;;; lsp-bridge-jdtls.el --- Provide jdtls configuration dynamically -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'json)

(defconst lsp-bridge-jdtls-workspace-file-name "jdtls.json"
  "Name of the generated jdtls configuration file.")

(defcustom lsp-bridge-jdtls-workspace
  (expand-file-name "~/.cache/lsp-bridge-jdtls")
  "LSP-Bridge jdtls workspace directory.")  :type 'directory


(defvar lsp-bridge-jdtls-default-file
  (expand-file-name
   (if (memq system-type '(cygwin windows-nt ms-dos))
       "langserver/jdtls_windows.json"
     "langserver/jdtls.json")
   (file-name-directory load-file-name))
  "The default jdtls configuration file shipped with lsp-bridge.
The Windows variant is used on Windows systems.")

(defcustom lsp-bridge-jdtls-jvm-args '()
  "Specifies additional VM parameters for starting the Java language server.
E.g. Use `-javaagent:/home/user/.emacs.d/plugin/lombok.jar` to add lombok support")  :type '(repeat string)


(defun lsp-bridge-get-jdtls-server-by-project (project-path filepath)
  "Return the JDTLS configuration for PROJECT-PATH and FILEPATH."
  (let ((config-file (lsp-bridge-jdtls-config-file project-path filepath)))
    (if (not (file-exists-p config-file))
        (lsp-bridge-jdtls-init-config project-path filepath))
    config-file))


(defun lsp-bridge-jdtls-init-config (project-path filepath)
  "Initialize the JDTLS configuration for PROJECT-PATH and FILEPATH."
  (let* ((json-object-type 'plist)
         (config (json-read-file lsp-bridge-jdtls-default-file))
         (config-file (lsp-bridge-jdtls-config-file project-path filepath))
         (data-directory (lsp-bridge-jdtls-project-data-dir project-path filepath)))

    ;; An additional JVM option (can be used multiple times. Note, use with equal sign. For example: --jvm-arg=-Dlog.level=ALL
    (when (listp lsp-bridge-jdtls-jvm-args)
      (let ((jvm-args (mapcar (lambda (arg) (concat "--jvm-arg=" arg))
                              lsp-bridge-jdtls-jvm-args)))

        (plist-put config :command (vconcat (plist-get config :command)
                                            jvm-args))))

    ;; Add the `-data` parameter to the startup parameter
    (plist-put config :command (vconcat (plist-get config :command) `("-data" ,data-directory)))

    "Create parent directory if not exists while visiting file."
    (make-directory (file-name-directory config-file) t)

    (with-temp-file config-file
      (insert (json-encode config)))))

(defun lsp-bridge-jdtls-cache-dir (project-path filepath)
  "Return the JDTLS cache directory for PROJECT-PATH and FILEPATH."
  (if (lsp-bridge-jdtls-single-file-mode? project-path filepath)
      (lsp-bridge-jdtls-single-file-cache-dir filepath)
    (lsp-bridge-jdtls-project-cache-dir project-path)))

(defun lsp-bridge-jdtls-project-cache-dir (project-path)
  "Return the JDTLS cache directory used for PROJECT-PATH."
  (let ((project-name (file-name-nondirectory project-path))
        (project-hash (md5 project-path)))
    (expand-file-name (concat project-name "-" project-hash)
                      lsp-bridge-jdtls-workspace)))

(defun lsp-bridge-jdtls-single-file-cache-dir (filepath)
  "Return the cache directory of the single file FILEPATH."
  (let ((single-file-hash (md5 filepath)))
    (expand-file-name single-file-hash
                      lsp-bridge-jdtls-workspace)))

(defun lsp-bridge-jdtls-config-file (project-path filepath)
  "Return the JDTLS configuration file path for PROJECT-PATH and FILEPATH."
  (expand-file-name lsp-bridge-jdtls-workspace-file-name
                    (lsp-bridge-jdtls-cache-dir project-path filepath)))

(defun lsp-bridge-jdtls-project-data-dir (project-path filepath)
  "Return the JDTLS data directory for PROJECT-PATH and FILEPATH."
  (expand-file-name "jdtls-data"
                    (lsp-bridge-jdtls-cache-dir project-path filepath)))


(defun lsp-bridge-jdtls-single-file-mode? (project-path filepath)
  "Return non-nil when PROJECT-PATH and FILEPATH name the same file."
  (lsp-bridge-path-equal project-path filepath))

(dolist (hook (list
               'java-mode-hook
               'java-ts-mode-hook))
  (add-hook hook (lambda () (setq-local lsp-bridge-get-single-lang-server-by-project 'lsp-bridge-get-jdtls-server-by-project))))

(defun lsp-bridge-jdtls-apply-workspace-edit (action &optional temp-buffer)
  "Handle the java.apply.workspaceEdit command in ACTION.
TEMP-BUFFER, when given, is the buffer the edits are applied to."
  (let ((arguments (plist-get action :arguments)))
    (dolist (argument arguments)
      (lsp-bridge-workspace-apply-edit argument temp-buffer))))

(defun lsp-bridge-jdtls-override-methods-prompt (action &optional temp-buffer)
  "Handle the java.action.overrideMethodsPrompt command in ACTION.
TEMP-BUFFER, when given, receives the list of overridable methods."
  (let ((argument (car (plist-get action :arguments))))
    (lsp-bridge-call-file-api "jdtls_list_overridable_methods" argument)))

(defun lsp-bridge-jdtls-add-overridable-methods (resp-json)
  "Handle a java/listOverridableMethods response in RESP-JSON.
The response is turned into the arguments of the
java/addOverridableMethods command, which is then sent back."
  ;; NOTE: The reason using json-string directly is that lsp-bridge elisp object <-> python object is still unreliable
  (when-let* ((resp (json-parse-string resp-json :object-type 'plist))
              (response (plist-get resp :response))
              (context (plist-get resp :context))
              (methods (plist-get response :methods))
              (menu-items (mapcar (lambda (method)
                                    (let* ((name (plist-get method :name))
                                           (parameters (plist-get method :parameters))
                                           (class (plist-get method :declaringClass)))
                                      (cons (format "%s(%s) class: %s" name (string-join parameters ", ") class) method)))
                                  methods))
              (selected-methods (cl-map 'vector
                                        (lambda (choice) (alist-get choice menu-items nil nil 'equal))
                                        (delete-dups
                                         (completing-read-multiple "overridable methods: " menu-items))))
              (params (list :overridableMethods selected-methods :context context)))
    (lsp-bridge-call-file-api "jdtls_add_overridable_methods" (json-serialize params))))


(provide 'lsp-bridge-jdtls)
;;; lsp-bridge-jdtls.el ends here
