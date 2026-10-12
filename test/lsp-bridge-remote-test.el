;;; lsp-bridge-remote-test.el --- Naming and decoding remote files  -*- lexical-binding: t; -*-

;;; Commentary:
;; Files opened over TRAMP are named by four components, and what the user types in
;; the minibuffer to ask for one is recognised by a regexp.  Both are text, and the
;; answers that arrive for such a file are base64, which is decoded before it is
;; read.  Those three are checked here; everything else in the module opens a
;; connection, talks to a peer on the other side of it, or keeps TRAMP's own state
;; in step, and needs that peer to exist.

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

(provide 'lsp-bridge-remote-test)
;;; lsp-bridge-remote-test.el ends here
