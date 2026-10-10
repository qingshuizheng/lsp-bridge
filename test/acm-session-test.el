;;; acm-session-test.el --- Tests for the per-frame popup state  -*- lexical-binding: t; -*-

;;; Commentary:

;; A session holds the popup state of one parent frame: its child frames, the
;; buffers they display, where the menu was popped up and the caches that keep
;; the menu from recalculating its size.  Two frames must not share any of it -
;; that is the whole point of the file - so these tests work with a session key
;; of their own: a second real frame cannot be created in batch Emacs, and the
;; hash table is keyed with `eq' by whatever the caller passes.
;;
;;   Emacs -Q --batch -L . -L acm -L test -l test/acm-session-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'acm-session)

(defvar acm-session-test--key 'acm-session-test-frame
  "A stand-in for the parent frame of a session under test.")

(ert-deftest acm-session-test-identity ()
  "One session per key, created on demand and reused afterwards."
  (let ((acm-sessions (make-hash-table :test 'eq)))
    (let ((first (acm-session acm-session-test--key)))
      (should (acm-session-p first))
      (should (eq first (acm-session acm-session-test--key)))
      (should (eq first (gethash acm-session-test--key acm-sessions))))
    ;; Another key is another session ...
    (should-not (eq (acm-session acm-session-test--key)
                    (acm-session 'acm-session-test-other-frame)))
    ;; ... and the default key is the frame of the current buffer.
    (should (eq (acm-session) (acm-session (acm-session--frame))))
    ;; The two keys above plus the frame of this buffer.
    (should (= (hash-table-count acm-sessions) 3))))

(ert-deftest acm-session-test-slots-are-independent ()
  "Storing state in one session leaves the other one alone."
  (let ((acm-sessions (make-hash-table :test 'eq))
        (other 'acm-session-test-other-frame))
    (acm-popup-set 'menu-max-length-cache 42 acm-session-test--key)
    (acm-popup-set 'doc-hide-p t acm-session-test--key)
    (should (= (acm-popup menu-max-length-cache acm-session-test--key) 42))
    (should (acm-popup doc-hide-p acm-session-test--key))
    ;; The session of the other frame still has its defaults.
    (should (= (acm-popup menu-max-length-cache other) 0))
    (should-not (acm-popup doc-hide-p other))
    ;; The menu and documentation buffers are per session as well.
    (should-not (eq (acm-popup menu-buffer acm-session-test--key)
                    (acm-popup menu-buffer other)))
    (should (buffer-live-p (acm-popup menu-buffer acm-session-test--key)))))

(ert-deftest acm-session-test-current-frame ()
  "The default session belongs to the frame of the current buffer.
A window of a popup frame resolves to its parent, since a child frame never
owns a session."
  (let ((acm-sessions (make-hash-table :test 'eq)))
    (should (eq (acm-session--frame) (selected-frame)))
    ;; A child frame resolves to its parent.
    (cl-letf (((symbol-function 'window-frame) (lambda (_window) 'child-frame))
              ((symbol-function 'frame-parent) (lambda (frame)
                                                 (if (eq frame 'child-frame)
                                                     'parent-frame
                                                   nil))))
      (should (eq (acm-session--frame) 'parent-frame)))))

(ert-deftest acm-session-test-drop ()
  "Dropping a session deletes its frames and buffers and forgets it."
  (let ((acm-sessions (make-hash-table :test 'eq))
        (deleted nil)
        (key 'acm-session-test-drop-frame))
    (let ((session (acm-session key)))
      (setf (acm-session-menu-frame session) 'menu-child
            (acm-session-doc-frame session) 'doc-child)
      (cl-letf (((symbol-function 'frame-live-p) (lambda (_frame) t))
                ((symbol-function 'delete-frame) (lambda (frame) (push frame deleted))))
        (acm-session-drop key))
      (should (equal (sort (mapcar #'symbol-name deleted) #'string<)
                     '("doc-child" "menu-child")))
      (should-not (buffer-live-p (acm-session-menu-buffer session)))
      (should-not (buffer-live-p (acm-session-doc-buffer session)))
      (should-not (gethash key acm-sessions)))
    ;; Dropping a key without a session is harmless.
    (should-not (acm-session-drop 'acm-session-test-unknown-frame))))

(ert-deftest acm-session-test-delete-frame-hook ()
  "The hook drops the session of the frame that is being deleted."
  (let ((acm-sessions (make-hash-table :test 'eq))
        (key 'acm-session-test-hook-frame))
    (should (memq #'acm-session-delete-frame-hook delete-frame-functions))
    (acm-session key)
    (acm-session-delete-frame-hook key)
    (should-not (gethash key acm-sessions))))

(ert-deftest acm-session-test-popup-buffer-p ()
  "The popup buffers of every session are recognised."
  (let ((acm-sessions (make-hash-table :test 'eq))
        (key 'acm-session-test-buffer-frame)
        (other 'acm-session-test-buffer-other))
    (let ((menu (acm-popup menu-buffer key))
          (doc (acm-popup doc-buffer key)))
      (should (acm-session-popup-buffer-p menu))
      (should (acm-session-popup-buffer-p doc))
      ;; The buffers of another session count as well.
      (should (acm-session-popup-buffer-p (acm-popup menu-buffer other)))
      (should-not (acm-session-popup-buffer-p (get-buffer-create "*acm-session-test-user*")))
      (should-not (acm-session-popup-buffer-p (current-buffer))))))

(ert-deftest acm-session-test-all ()
  "Every session of every frame is reachable, for a theme change."
  (let ((acm-sessions (make-hash-table :test 'eq)))
    (should-not (acm-session-all))
    (let ((one (acm-session 'acm-session-test-all-one))
          (two (acm-session 'acm-session-test-all-two)))
      (should (equal (sort (mapcar (lambda (session)
                                     (buffer-name (acm-session-menu-buffer session)))
                                   (acm-session-all))
                           #'string<)
                     (sort (list (buffer-name (acm-session-menu-buffer one))
                                 (buffer-name (acm-session-menu-buffer two)))
                           #'string<))))))

(provide 'acm-session-test)
;;; acm-session-test.el ends here
