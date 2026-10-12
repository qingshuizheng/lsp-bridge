;;; lsp-bridge-epc-test.el --- The deferred queue behind every request  -*- lexical-binding: t; -*-

;;; Commentary:
;; The deferred objects in `lsp-bridge-epc.el' are what every request of this
;; package is built out of: a callback, an errorback, a link to the next object,
;; and a value once it has run.  The queue they wait in is drained by a timer, one
;; object at a time, in the order they were posted.  None of that needs a server or
;; a process - it is a small state machine - so the state machine is asked here:
;; what a new object looks like, what running a callback leaves behind, how a chain
;; passes its value along, where an error in a callback goes, and what connecting
;; two objects does when one of them has already finished.
;;
;; The timer is not waited for: the tests call `lsp-bridge-deferred-worker' the way
;; the timer would.  The EPC half of the same file - the sockets, the process
;; filter and the messages that go over them - is not here, since it needs a peer.

;;; Code:

(require 'ert)
(require 'lsp-bridge-epc)

(ert-deftest lsp-bridge-epc-test-new-object-defaults ()
  "A new object has the default callback, errorback and cancel, and no value."
  (let ((deferred (lsp-bridge-deferred-new)))
    (should (eq (lsp-bridge-deferred-object-callback deferred) 'identity))
    (should (eq (lsp-bridge-deferred-object-errorback deferred)
                'lsp-bridge-deferred-resignal))
    (should (eq (lsp-bridge-deferred-object-cancel deferred)
                'lsp-bridge-deferred-default-cancel))
    (should-not (lsp-bridge-deferred-object-next deferred))
    (should-not (lsp-bridge-deferred-object-status deferred))
    (should-not (lsp-bridge-deferred-object-value deferred)))
  (should (equal (funcall (lsp-bridge-deferred-object-callback
                           (lsp-bridge-deferred-new (lambda (arg) (1+ arg))))
                          41)
                 42)))

(ert-deftest lsp-bridge-epc-test-callback-runs-and-remembers-its-value ()
  "Running a callback gives its result, and the object keeps it."
  (let ((seen nil))
    (let ((deferred (lsp-bridge-deferred-new (lambda (arg) (setq seen arg) (* 2 arg)))))
      (should (equal (lsp-bridge-deferred-callback deferred 21) 42))
      (should (equal seen 21))
      (should (eq (lsp-bridge-deferred-object-status deferred) 'ok))
      (should (equal (lsp-bridge-deferred-object-value deferred) 42)))))

(ert-deftest lsp-bridge-epc-test-chained-callbacks ()
  "A chained object is given the value of the one before it."
  (let ((lsp-bridge-deferred-queue nil)
        (values nil))
    (let* ((first (lsp-bridge-deferred-new (lambda (arg) (push arg values) (1+ arg))))
           (second (lsp-bridge-deferred-nextc first
                                              (lambda (arg) (push arg values) (* 10 arg)))))
      (lsp-bridge-deferred-callback first 1)
      (should (equal (nreverse values) '(1)))
      (lsp-bridge-deferred-worker)
      (should (equal (nreverse values) '(1 2)))
      (should (equal (lsp-bridge-deferred-object-value second) 20)))))

(ert-deftest lsp-bridge-epc-test-queue-is-first-in-first-out ()
  "The queue is drained in the order things were posted to it."
  (let ((lsp-bridge-deferred-queue nil)
        order)
    (let ((first (lsp-bridge-deferred-new (lambda (_arg) (push 'first order))))
          (second (lsp-bridge-deferred-new (lambda (_arg) (push 'second order)))))
      (lsp-bridge-deferred-post-task first 'ok nil)
      (lsp-bridge-deferred-post-task second 'ok nil)
      (should (equal (length lsp-bridge-deferred-queue) 2))
      (lsp-bridge-deferred-worker)
      (should (equal order '(first)))
      (should (equal (length lsp-bridge-deferred-queue) 1))
      (lsp-bridge-deferred-worker)
      (should (equal order '(second first)))
      (should-not lsp-bridge-deferred-queue))))

(ert-deftest lsp-bridge-epc-test-an-error-goes-to-the-errorback-of-the-chain ()
  "A callback that signals passes the error to the errorback after it."
  (let ((lsp-bridge-deferred-queue nil)
        caught)
    (let ((deferred (lsp-bridge-deferred-new (lambda (_arg) (error "boom")))))
      (lsp-bridge-deferred-error deferred (lambda (err) (setq caught err)))
      (lsp-bridge-deferred-callback deferred nil)
      (lsp-bridge-deferred-worker)
      (should caught)
      (should (equal (error-message-string caught) "boom")))))

(ert-deftest lsp-bridge-epc-test-an-error-without-a-chain-is-kept ()
  "An error in a callback of an object with no next one is kept on it."
  (let ((deferred (lsp-bridge-deferred-new (lambda (_arg) (error "boom")))))
    (lsp-bridge-deferred-callback deferred nil)
    (should (eq (lsp-bridge-deferred-object-status deferred) 'ng))
    (should (equal (error-message-string (lsp-bridge-deferred-object-value deferred))
                   "boom"))))

(ert-deftest lsp-bridge-epc-test-with-no-callback-at-all-an-error-is-resignaled ()
  "An object with neither callback nor errorback re-signals, and passes values on."
  (let ((deferred (make-lsp-bridge-deferred-object :callback nil :errorback nil)))
    (should (equal (lsp-bridge-deferred-exec-task deferred 'ok 7) 7))
    (should-error (lsp-bridge-deferred-exec-task deferred 'ng "boom") :type 'error)))

(ert-deftest lsp-bridge-epc-test-set-next-runs-an-object-that-already-finished ()
  "Connecting to an object that already has a value runs the chain at once."
  (let ((value nil))
    (let* ((finished (lsp-bridge-deferred-new (lambda (arg) (* 3 arg))))
           (next (lsp-bridge-deferred-new (lambda (arg) (setq value arg)))))
      (lsp-bridge-deferred-callback finished 14)
      (should (eq (lsp-bridge-deferred-object-status finished) 'ok))
      (lsp-bridge-deferred-set-next finished next)
      (should (equal value 42))
      ;; The status is what is forgotten; the value of what already ran stays.
      (should-not (lsp-bridge-deferred-object-status finished))
      (should (equal (lsp-bridge-deferred-object-value finished) 42)))))

(ert-deftest lsp-bridge-epc-test-a-callback-returning-an-object-is-chained ()
  "A callback that returns another object links it into the chain."
  (let ((lsp-bridge-deferred-queue nil)
        seen)
    (let* ((inner (lsp-bridge-deferred-new (lambda (arg) (* 2 arg))))
           (outer (lsp-bridge-deferred-new (lambda (_arg) inner))))
      (lsp-bridge-deferred-nextc outer (lambda (arg) (setq seen arg)))
      (lsp-bridge-deferred-callback outer 5)
      (should-not seen)
      ;; The object the callback returned is the one that has to finish, and the
      ;; step after it is posted like every other step of a chain.
      (lsp-bridge-deferred-callback inner 5)
      (lsp-bridge-deferred-worker)
      (should (equal seen 10)))))

(ert-deftest lsp-bridge-epc-test-resignal ()
  "An error is raised as the condition it already is, or as a plain error."
  (should-error (lsp-bridge-deferred-resignal "boom") :type 'error)
  (should-error (lsp-bridge-deferred-resignal '(wrong-type-argument integerp "x"))
                :type 'wrong-type-argument)
  (should-error (lsp-bridge-deferred-resignal 42) :type 'error))

(provide 'lsp-bridge-epc-test)
;;; lsp-bridge-epc-test.el ends here
