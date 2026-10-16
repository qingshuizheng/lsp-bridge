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


(ert-deftest acm-backend-telega-test-precode-accepts-the-fence-in-the-keyword ()
  "The fence may be part of the keyword, as the string style gives it."
  (acm-backend-telega-test--with-telega
    (with-temp-buffer
      (setq major-mode 'telega-chat-mode)
      (cl-letf (((symbol-function 'telega-completions--language-names)
                 (lambda () '("python" "purescript" "rust"))))
        (insert "```py")
        (goto-char (point-max))
        (cl-flet ((labels (keyword)
                    (mapcar (lambda (c) (plist-get c :label))
                            (acm-backend-telega-precode-candidates keyword))))
          (should (equal '("python") (labels "```py")))
          (should (equal '("python") (labels "py"))))))))

(ert-deftest acm-backend-telega-test-precode-expansion-keeps-the-fence ()
  "Completing a language replaces the fence only when the prefix held it."
  (acm-backend-telega-test--with-telega
    (cl-flet ((complete (start)
                (with-temp-buffer
                  (insert "```py")
                  (goto-char (point-max))
                  (acm-backend-telega-candidate-expand (list :label "python" :precode t) start nil)
                  (buffer-string))))
      ;; the string style: the prefix starts at the fence, so it is inserted back
      (should (equal "```python\n\n```" (complete (point-min))))
      ;; the symbol styles: the fence is already before the prefix
      (should (equal (complete (point-min)) (complete 4))))))


(ert-deftest acm-backend-telega-test-the-people-the-buffer-has-rendered ()
  "Names telega rendered in the buffer are candidates, member list or not.
Telega wraps a rendered name in a button carrying the sender object under
`:telega-link'; that object is what the candidate is built from, so a member
without a username completes too.  A name merely written in the text is
offered as the text it is."
  (with-temp-buffer
    (insert "hello ")
    (let ((start (point)))
      (insert "Alice")
      (add-text-properties start (point) (list :telega-link (cons 'sender 'alice-user))))
    (insert " and @bob and again Alice\n")
    (let ((start (point)))
      (insert "Alice")
      (add-text-properties start (point) (list :telega-link (cons 'sender 'alice-user))))
    (cl-letf (((symbol-function 'telega-msg-sender-username)
               (lambda (user &optional _with-at)
                 (pcase user
                   ('alice-user "@alice")
                   (_ nil))))
              ((symbol-function 'telega-msg-sender-title)
               (lambda (user &rest _) (format "%s" user))))
      (let* ((candidates (acm-backend-telega--buffer-mention-candidates))
             (labels (mapcar (lambda (item) (plist-get item :label)) candidates)))
        ;; Alice is rendered twice and is one candidate; bob is only in the text.
        (should (equal labels '("@alice" "@bob")))
        ;; The rendered name keeps its sender object, which is what makes the
        ;; mention entity possible for a member without a username.
        (should (eq (plist-get (car candidates) :member) 'alice-user))
        ;; The one read out of the text does not have one.
        (should-not (plist-get (cadr candidates) :member))))))

(ert-deftest acm-backend-telega-test-a-name-read-from-the-text-is-inserted ()
  "A candidate with no sender object is inserted as the text its label holds."
  (with-temp-buffer
    (insert "@bo")
    (acm-backend-telega-candidate-expand
     (list :key "@bob" :label "@bob" :backend "telega")
     (point-min))
    (should (equal (buffer-string) "@bob "))))

(ert-deftest acm-backend-telega-test-reading-the-buffer-is-bounded ()
  "A long chat costs what the end of it costs, not what the whole of it costs."
  (with-temp-buffer
    (let ((acm-backend-telega-buffer-scan-limit 40))
      ;; A name far from the end, then more than the limit of message text.
      (insert "@old ")
      (insert (make-string 200 ?x))
      ;; A trailing space, so that the last name is not the prefix being typed:
      ;; an occurrence that ends at point is taken for that and skipped.
      (insert " @fresh ")
      (let ((labels (mapcar (lambda (item) (plist-get item :label))
                            (acm-backend-telega--buffer-mention-candidates))))
        ;; Only what falls inside the window is read.
        (should (member "@fresh" labels))
        (should-not (member "@old" labels))))))

(ert-deftest acm-backend-telega-test-a-failing-buffer-read-still-leaves-the-member-list ()
  "The mention backend never takes the other completions down with it.
Whatever the buffer read meets, the candidates the member list answered are
still returned - the same list the emoji, hashtags and slash commands are
appended to after it."
  (with-temp-buffer
    (insert "@a")
    (let ((acm-backend-telega-items (list (list :key "@alice" :label "@alice"
                                               :annotation "member" :backend "telega"))))
      (cl-letf (((symbol-function 'acm-backend-telega-enabled-p) (lambda () t))
                ((symbol-function 'acm-backend-telega--typed-prefix-p) (lambda (&rest _) t))
                ((symbol-function 'acm-backend-telega--buffer-mention-candidates)
                 (lambda () (error "the buffer was not the shape it expected"))))
        (should (equal (mapcar (lambda (item) (plist-get item :label))
                               (acm-backend-telega-mention-candidates "a"))
                       '("@alice")))))))

(provide 'acm-backend-telega-test)
;;; acm-backend-telega-test.el ends here
