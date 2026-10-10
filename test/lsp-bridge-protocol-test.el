;;; lsp-bridge-protocol-test.el --- Tests for the protocol handshake  -*- lexical-binding: t; -*-

;;; Commentary:

;; The Emacs half of the protocol is two things: `lsp-bridge--handshake', which
;; compares the version this file declares with the one the Python side answers
;; and refuses to go on when they differ, and `lsp-bridge-diagnose--report',
;; which turns the facts of both halves into the text a bug report carries.
;; Neither needs a running server here: the handshake is tested against a
;; stubbed `lsp-bridge-call-sync' and the report against a state plist.
;;
;;   Emacs -Q --batch -L . -L acm -L test -l test/lsp-bridge-protocol-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'lsp-bridge)

(defun lsp-bridge-protocol-test--handshake-json (protocol-version &optional compatible)
  "Return a handshake reply for PROTOCOL-VERSION.
COMPATIBLE defaults to whether the version matches this file's."
  (json-serialize
   (list :protocol_version protocol-version
         :compatible (if (null compatible)
                         (= protocol-version lsp-bridge-protocol-version)
                       compatible)
         :capabilities ["completion" "diagnose"]
         :python_version "3.13.5"
         :pid 4242)))

(defmacro lsp-bridge-protocol-test--with-sync (reply &rest body)
  "Run BODY with `lsp-bridge-call-sync' answering REPLY.
REPLY is either a value or a form that is evaluated; a `:error' cons signals
that error from the call, the way an older server would."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'lsp-bridge-call-sync)
              (lambda (&rest _args)
                (let ((reply ,reply))
                  (if (and (consp reply) (eq (car reply) :error))
                      (signal 'error (list (cdr reply)))
                    reply)))))
     ,@body))

;;; The handshake

(ert-deftest lsp-bridge-protocol-test-handshake-accepts-match ()
  "A server that speaks this protocol version is accepted."
  (let ((lsp-bridge--server-handshake nil)
        (lsp-bridge--protocol-error nil))
    (lsp-bridge-protocol-test--with-sync
        (lsp-bridge-protocol-test--handshake-json lsp-bridge-protocol-version)
      (should (lsp-bridge--handshake)))
    (should (eq (alist-get 'compatible lsp-bridge--server-handshake) t))
    (should (equal (alist-get 'protocol_version lsp-bridge--server-handshake)
                   lsp-bridge-protocol-version))
    (should-not lsp-bridge--protocol-error)))

(ert-deftest lsp-bridge-protocol-test-handshake-rejects-mismatch ()
  "A server that speaks another protocol version is reported, not used."
  (let ((lsp-bridge--server-handshake nil)
        (lsp-bridge--protocol-error nil))
    (lsp-bridge-protocol-test--with-sync
        (lsp-bridge-protocol-test--handshake-json (1+ lsp-bridge-protocol-version) nil)
      (should-not (lsp-bridge--handshake)))
    (should (string-match-p "protocol mismatch"
                            lsp-bridge--protocol-error))
    (should (string-match-p (number-to-string lsp-bridge-protocol-version)
                            lsp-bridge--protocol-error))
    ;; The reply is kept, so that the diagnose command can report it.
    (should lsp-bridge--server-handshake)))

(ert-deftest lsp-bridge-protocol-test-handshake-reports-a-silent-server ()
  "A server that does not answer - an older one, say - is reported."
  (let ((lsp-bridge--server-handshake nil)
        (lsp-bridge--protocol-error nil))
    (lsp-bridge-protocol-test--with-sync '(:error "Unknown method handshake")
      (should-not (lsp-bridge--handshake)))
    (should (string-match-p "did not answer the handshake"
                            lsp-bridge--protocol-error))
    (should-not lsp-bridge--server-handshake)))

(ert-deftest lsp-bridge-protocol-test-handshake-reports-an-unreadable-reply ()
  "A reply that is not JSON is reported as such."
  (let ((lsp-bridge--server-handshake nil)
        (lsp-bridge--protocol-error nil))
    (lsp-bridge-protocol-test--with-sync "not json at all"
      (should-not (lsp-bridge--handshake)))
    (should (string-match-p "not JSON" lsp-bridge--protocol-error))))

(ert-deftest lsp-bridge-protocol-test-handshake-announces-the-version ()
  "The handshake is asked with the version this file declares."
  (let ((asked nil)
        (lsp-bridge--server-handshake nil)
        (lsp-bridge--protocol-error nil))
    (cl-letf (((symbol-function 'lsp-bridge-call-sync)
               (lambda (&rest args)
                 (setq asked args)
                 (lsp-bridge-protocol-test--handshake-json lsp-bridge-protocol-version))))
      (lsp-bridge--handshake))
    (should (equal (car asked) "handshake"))
    (should (equal (cadr asked) lsp-bridge-protocol-version))))

;;; The report

(ert-deftest lsp-bridge-protocol-test-report-lists-both-halves ()
  "The report names the two halves, the handshake and the server facts."
  (let ((report (lsp-bridge-diagnose--report
                 (list :elisp-file "/tmp/lsp-bridge.el"
                       :protocol-version lsp-bridge-protocol-version
                       :python-args '("uv" "run" "/tmp/lsp_bridge.py" "1234")
                       :python-file "/tmp/lsp_bridge.py"
                       :process t
                       :handshake '((protocol_version . 2)
                                    (compatible . t)
                                    (capabilities . (completion diagnose))
                                    (python_version . "3.13.5")
                                    (pid . 4242))
                       :handshake-error nil
                       :server '((lsp_server_count . 3)
                                 (file_action_count . 5)
                                 (python_version . "3.13.5")
                                 (pid . 4242))
                       :log-buffer "*lsp-bridge*"
                       :log-lines 120
                       :log-level 'debug))))
    (should (string-match-p "LSP-Bridge diagnose" report))
    (should (string-match-p "/tmp/lsp-bridge.el" report))
    (should (string-match-p "protocol 2" report))
    (should (string-match-p "uv run /tmp/lsp_bridge.py 1234" report))
    (should (string-match-p "Process      : live" report))
    (should (string-match-p "agreed" report))
    (should (string-match-p "completion, diagnose" report))
    (should (string-match-p "3 language server(s), 5 tracked file(s)" report))
    (should (string-match-p "\\*lsp-bridge\\* (120 lines, level debug)" report))))

(ert-deftest lsp-bridge-protocol-test-report-shows-a-failed-handshake ()
  "A failed handshake is reported instead of the agreed facts."
  (let ((report (lsp-bridge-diagnose--report
                 (list :elisp-file "/tmp/lsp-bridge.el"
                       :protocol-version lsp-bridge-protocol-version
                       :python-args '("python3" "/tmp/lsp_bridge.py")
                       :process nil
                       :handshake nil
                       :handshake-error "protocol mismatch: Emacs speaks 2, the server speaks 1"
                       :server nil
                       :log-buffer nil
                       :log-lines nil
                       :log-level nil))))
    (should (string-match-p "protocol mismatch" report))
    (should-not (string-match-p "agreed" report))
    (should (string-match-p "Process      : not running" report))
    (should (string-match-p "Server facts : unavailable" report))
    (should (string-match-p "no log buffer" report))))

(provide 'lsp-bridge-protocol-test)
;;; lsp-bridge-protocol-test.el ends here
