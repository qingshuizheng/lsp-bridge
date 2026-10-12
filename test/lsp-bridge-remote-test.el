;;; lsp-bridge-remote-test.el --- Naming and decoding remote files  -*- lexical-binding: t; -*-

;;; Commentary:
;; Files opened over TRAMP are named by four components, and what the user types in
;; the minibuffer to ask for one is recognised by a regexp.  Both are text, and the
;; answers that arrive for such a file are base64, which is decoded before it is
;; read.  Those three are checked here, and so are the reading of a file that may
;; not exist yet, the buffer a host is looked up by, and the two alists that
;; remember a connection.  What is left opens a connection or talks to a peer on
;; the other side of it, and needs that peer to exist.

;;; Code:

(require 'ert)
(require 'lsp-bridge)
(require 'lsp-bridge-remote)

(ert-deftest lsp-bridge-remote-test-construct-tramp-file-name ()
  "The components become the TRAMP file name, with the optional parts left out."
  (should (equal (lsp-bridge-construct-tramp-file-name "ssh" "user" "1.2.3.4" "2222" "/tmp/a.py")
                 "/ssh:user@1.2.3.4#2222:/tmp/a.py"))
  (should (equal (lsp-bridge-construct-tramp-file-name "ssh" nil "1.2.3.4" nil "~/a.py")
                 "/ssh:1.2.3.4:~/a.py")))

(ert-deftest lsp-bridge-remote-test-decode-base64 ()
  "A base64 answer is decoded as the UTF-8 text it holds."
  (should (equal (lsp-bridge-decode-base64 "aGVsbG8=") "hello"))
  (should (equal (lsp-bridge-decode-base64 "") ""))
  ;; The text of a file, not of the terminal it was written on.
  (should (equal (lsp-bridge-decode-base64
                      (base64-encode-string (encode-coding-string "你好" 'utf-8) t))
                     "你好")))

(ert-deftest lsp-bridge-remote-test-file-pattern-matches ()
  "A remote request is an address, a port and a path."
  (should (string-match-p lsp-bridge-remote-file-pattern "1.2.3.4:~/a.py"))
  (should (string-match-p lsp-bridge-remote-file-pattern "1.2.3.4:/tmp/a.py"))
  (should (string-match-p lsp-bridge-remote-file-pattern "user@1.2.3.4:22:/tmp/a.py"))
  (should (string-match-p lsp-bridge-remote-file-pattern "1.2.3.4:2222:~/a.py")))

(ert-deftest lsp-bridge-remote-test-file-pattern-does-not-match ()
  "An ordinary path or file name is not a remote request."
  (should-not (string-match-p lsp-bridge-remote-file-pattern "notes.txt"))
  (should-not (string-match-p lsp-bridge-remote-file-pattern "/tmp/a.py"))
  (should-not (string-match-p lsp-bridge-remote-file-pattern "1.2.3:/tmp/a.py"))
  (should-not (string-match-p lsp-bridge-remote-file-pattern "host:~/a.py")))

(ert-deftest lsp-bridge-remote-test-open-or-create-file ()
  "An existing file is read, and a missing one is made along with its parents."
  (let ((root (make-temp-file "lsp-bridge-remote-test" t)))
    (unwind-protect
        (let ((existing (expand-file-name "a.txt" root))
              (missing (expand-file-name "deep/inner/b.txt" root)))
          (with-temp-file existing (insert "contents\n"))
          (should (equal (lsp-bridge-open-or-create-file existing) "contents\n"))
          (should (equal (lsp-bridge-open-or-create-file missing) ""))
          (should (file-exists-p missing))
          (should (file-directory-p (file-name-directory missing))))
      (delete-directory root t))))

(ert-deftest lsp-bridge-remote-test-get-match-buffer-by-filehost ()
  "The buffer whose remote host is the one asked for is the one returned."
  (let ((buffer (generate-new-buffer " *lsp-bridge-remote-test*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local lsp-bridge-remote-file-host "1.2.3.4"))
          (should (eq (lsp-bridge-get-match-buffer-by-filehost "1.2.3.4") buffer))
          (should-not (lsp-bridge-get-match-buffer-by-filehost "5.6.7.8")))
      (kill-buffer buffer))))

(ert-deftest lsp-bridge-remote-test-update-tramp-file-info ()
  "A connection is remembered once, and the buffer of the file is marked as remote.
The marking is done inside `lsp-bridge--conditional-update-tramp-file-info', which
is a macro that runs its body in the buffer the file belongs to: a buffer that is
not marked as that remote file's is not the one, so the test marks it first, the
way opening a remote file leaves it."
  (let ((lsp-bridge-tramp-alias-alist nil)
        (lsp-bridge-tramp-connection-info nil)
        (file (make-temp-file "lsp-bridge-remote-test" nil ".py")))
    (unwind-protect
        (with-current-buffer (find-file-noselect file)
          ;; The buffer the macro looks for, marked as the remote file itself.
          (setq-local lsp-bridge-remote-file-path "/tmp/a.py")
          (setq-local lsp-bridge-remote-file-host "1.2.3.4")
          (dotimes (_ 2)
            (lsp-bridge-update-tramp-file-info "/ssh:user@1.2.3.4:/tmp/a.py" "conn"
                                               "ssh" "user" "1.2.3.4" "/tmp/a.py"))
          ;; Remembered once, however many times it is told.
          (should (equal (length lsp-bridge-tramp-alias-alist) 1))
          (should (equal (length lsp-bridge-tramp-connection-info) 1))
          (should lsp-bridge-remote-file-flag)
          (should (equal lsp-bridge-remote-file-tramp-method "ssh"))
          (should (equal lsp-bridge-remote-file-user "user"))
          (should (equal lsp-bridge-remote-file-path "/tmp/a.py"))
          (should lsp-bridge-tramp-sync-var))
      ;; The function installs a hook that tells the server the file is gone,
      ;; which there is no server for here, so the hook goes before the buffer does.
      (let ((buffer (get-file-buffer file)))
        (when buffer
          (with-current-buffer buffer
            (remove-hook 'kill-buffer-hook 'lsp-bridge-remote-kill-buffer t))
          (kill-buffer buffer)))
      (delete-file file))))

(provide 'lsp-bridge-remote-test)
;;; lsp-bridge-remote-test.el ends here
