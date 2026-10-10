;;; acm-backend-telega-test.el --- The telega backend's own logic  -*- lexical-binding: t -*-

;;; Commentary:
;; The telega sources need a real chat to produce candidates, but the two things
;; that decide whether the member list is ever fetched do not: the backend asks
;; for it when the slot still holds the `fetch' sentinel, and the async callback
;; must not abort when the buffer has left the chat in the meantime.  Both are
;; checked here, and both skip when telega is not installed (it is not a
;; dependency of the package, only of the machine that actually uses telega).

;;; Code:

(require 'ert)
(require 'cl-lib)

;; The backend is a part of the frontend: the filtering it does uses
;; `acm-candidate-sort-by-prefix' and friends.
(require 'acm)

(defmacro acm-backend-telega-test--with-telega (&rest body)
  "Run BODY with the telega backend loaded, or skip the test."
  (declare (indent 0) (debug t))
  `(progn
     (skip-unless (require 'telega nil t))
     (require 'acm-backend-telega)
     ,@body))

(ert-deftest acm-backend-telega-test-mention-asks-for-the-list ()
  "A chat whose list is still the sentinel asks for it while completing."
  (acm-backend-telega-test--with-telega
    (with-temp-buffer
      (setq major-mode 'telega-chat-mode)
      (setq-local acm-backend-telega-items 'fetch)
      (insert "@")
      (let (requested)
        (cl-letf (((symbol-function 'acm-backend-telega-enabled-p) (lambda () t))
                  ((symbol-function 'acm-backend-telega-update-items)
                   (lambda (&rest _)
                     (setq requested t)
                     (setq acm-backend-telega-items nil))))
          ;; the sentinel is not a candidate, so nothing is offered yet ...
          (should-not (acm-backend-telega-mention-candidates ""))
          ;; ... but the fetch has been started, which is what was missing.
          (should requested))))))

(ert-deftest acm-backend-telega-test-mention-filters-the-list ()
  "Once the list is there, the mention source filters it by the typed keyword."
  (acm-backend-telega-test--with-telega
    (with-temp-buffer
      (setq major-mode 'telega-chat-mode)
      ;; A candidate per member, built without telega's accessors so that this
      ;; checks the filtering and not the shape of a telega user object.
      (setq-local acm-backend-telega-items
                  (mapcar (lambda (name) (list :label name :annotation name))
                          '("alice" "bob")))
      (cl-letf (((symbol-function 'acm-backend-telega-enabled-p) (lambda () t)))
        ;; The source is called with point right after the typed keyword, so the
        ;; text in front of it is always "@" followed by that keyword.
        (cl-flet ((ask (keyword)
                    (erase-buffer)
                    (insert "@" keyword)
                    (goto-char (point-max))
                    (acm-backend-telega-mention-candidates keyword)))
          (should (= 2 (length (ask ""))))
          (should (equal '("alice")
                         (mapcar (lambda (c) (plist-get c :label)) (ask "ali"))))
          (should (= 1 (length (ask "bob")))))))))

(ert-deftest acm-backend-telega-test-mention-accepts-the-trigger-in-the-keyword ()
  "The trigger character may be part of the keyword, as the string style gives it."
  (acm-backend-telega-test--with-telega
    (with-temp-buffer
      (setq major-mode 'telega-chat-mode)
      (setq-local acm-backend-telega-items
                  (list (list :label "@alice" :annotation "Alice")))
      (cl-letf (((symbol-function 'acm-backend-telega-enabled-p) (lambda () t)))
        (cl-flet ((ask (keyword)
                    (erase-buffer)
                    (insert "@" (string-remove-prefix "@" keyword))
                    (goto-char (point-max))
                    (mapcar (lambda (c) (plist-get c :label))
                            (acm-backend-telega-mention-candidates keyword))))
          ;; `acm-input-bound-style' is "string": the prefix starts after the last
          ;; whitespace, so the "@" is handed over inside the keyword.
          (should (equal '("@alice") (ask "@al")))
          ;; the symbol/ascii styles hand over "al" alone, which always worked.
          (should (equal '("@alice") (ask "al")))
          (should-not (ask "zz")))))))


(provide 'acm-backend-telega-test)
;;; acm-backend-telega-test.el ends here
