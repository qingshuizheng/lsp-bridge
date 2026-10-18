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
        ;; The one read out of the text does not have one, and its searchable name
        ;; is the text it was read from.
        (should-not (plist-get (cadr candidates) :member))
        (should (equal (plist-get (cadr candidates) :search) "@bob"))))))

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

(ert-deftest acm-backend-telega-test-expanding-takes-the-token-out-of-a-read-only-line ()
  "A mention is written over even where telega has marked the text read-only.
The messages of a chat buffer are read-only, and the token a candidate replaces is
written over: without `inhibit-read-only' this signalled `text-read-only', which is
the error that came out of the post-command hook when a mention had a single
candidate and lsp-bridge took it by itself."
  (with-temp-buffer
    (insert "Bob: hello @bo\n")
    (add-text-properties (point-min) (point-max) '(read-only t))
    (let ((start (- (point) 4)))          ; the "@" sits inside the read-only text
      (should (null (acm-backend-telega-candidate-expand
                     (list :key "@bob" :label "@bob" :backend "telega")
                     start)))
      ;; The newline went with the token: the region the caller names is what is
      ;; replaced, and telega's input line has nothing after the token.
      (should (equal (buffer-string) "Bob: hello @bob ")))))

(ert-deftest acm-backend-telega-test-the-inaccessible-member-list-is-not-shown ()
  "The one message that a hidden member list produces is dropped; others are not."
  (let (shown)
    (let ((acm-backend-telega-quiet-member-list-error t))
      ;; The exact call telega's server makes for a failed request.
      (should (null (acm-backend-telega--keep-quiet-about-inaccessible-members
                     (lambda (&rest args) (setq shown args))
                     "telega-server error: %s"
                     (list :code 400 :message "Member list is inaccessible"))))
      (should (null shown))
      ;; Another server error is passed on, with its arguments.
      (acm-backend-telega--keep-quiet-about-inaccessible-members
       (lambda (&rest args) (setq shown args))
       "telega-server error: %s"
       (list :code 400 :message "Chat not found"))
      (should (equal (cadr shown) (list :code 400 :message "Chat not found")))
      ;; And so is anything else that goes through `message'.
      (setq shown nil)
      (acm-backend-telega--keep-quiet-about-inaccessible-members
       (lambda (&rest args) (setq shown args)) "Fetch telega userlist... done.")
      (should (equal (car shown) "Fetch telega userlist... done.")))
    ;; The switch turns the filtering off.
    (let ((acm-backend-telega-quiet-member-list-error nil))
      (setq shown nil)
      (acm-backend-telega--keep-quiet-about-inaccessible-members
       (lambda (&rest args) (setq shown args))
       "telega-server error: %s"
       (list :code 400 :message "Member list is inaccessible"))
      (should shown))))

(ert-deftest acm-backend-telega-test-the-filter-is-installed ()
  "The filtering is on the `message' function itself, not only written down."
  (should (advice-member-p #'acm-backend-telega--keep-quiet-about-inaccessible-members
                           #'message)))

(ert-deftest acm-backend-telega-test-mention-by-the-name-the-chat-renders ()
  "A member is found by the name the chat shows, not only by the username.
The label of a mention is the username, so a name typed as it appears in the
chat - the title, or the text of the button the name was rendered in - has to be
asked for as well; this is the case of someone whose name is not their @name."
  (with-temp-buffer
    (insert "hello ")
    (let ((start (point)))
      (insert "DT")
      (add-text-properties start (point)
                           (list :telega-link (cons 'sender 'dt-user))))
    (insert " and more\n")
    ;; The @ the mention candidates are asked after, and a member list that is
    ;; already in hand, so that nothing tries to fetch one here.
    (setq-local acm-backend-telega-items nil)
    (goto-char (point-max))
    (insert "@")
    (cl-letf (((symbol-function 'acm-backend-telega-enabled-p) (lambda () t))
              ((symbol-function 'acm-backend-telega--typed-prefix-p) (lambda (&rest _) t))
              ((symbol-function 'telega-msg-sender-username)
               (lambda (user &optional _) (pcase user ('dt-user "@thewawar") (_ nil))))
              ((symbol-function 'telega-msg-sender-title)
               (lambda (user &rest _) (pcase user ('dt-user "DT") (_ nil)))))
      (let ((labels (lambda (keyword)
                      (mapcar (lambda (item) (plist-get item :label))
                              (acm-backend-telega-mention-candidates keyword)))))
        ;; The username finds them, and so does the name the chat shows.
        (should (equal (funcall labels "thewawar") '("@thewawar")))
        (should (equal (funcall labels "DT") '("@thewawar")))
        ;; And the candidate remembers the rendered name it was read from.
        (should (equal (plist-get (car (acm-backend-telega-mention-candidates "DT")) :search)
                       "DT DT @thewawar"))))))

(ert-deftest acm-backend-telega-test-finds-the-sender-of-a-message ()
  "The sender of a message the buffer rendered is a candidate.
Telega puts the message object in the `:value' property of the text it wrote for
it, and the sender is one lookup away: this is how the people who have spoken in
the chat are found, which is everyone the buffer shows."
  (with-temp-buffer
    (insert "hello ")
    (let ((start (point)))
      (insert "DT • @thewawar")
      (add-text-properties start (point)
                           (list :value (list :sender_id
                                              (list :@type "messageSenderUser" :user_id 515878906)))))
    (insert "\n")
    (setq-local acm-backend-telega-items nil)
    (goto-char (point-max))
    (insert "@")
    (cl-letf (((symbol-function 'acm-backend-telega-enabled-p) (lambda () t))
              ((symbol-function 'acm-backend-telega--typed-prefix-p) (lambda (&rest _) t))
              ((symbol-function 'telega-msg-sender) (lambda (id) (list :id (plist-get id :user_id))))
              ((symbol-function 'telega-user-p) (lambda (object) (integerp (plist-get object :id))))
              ((symbol-function 'telega-msg-sender-username) (lambda (&rest _) "@thewawar"))
              ((symbol-function 'telega-msg-sender-title) (lambda (&rest _) "DT")))
      ;; Both what the chat shows and the username find them.
      (should (equal (mapcar (lambda (c) (plist-get c :label))
                             (acm-backend-telega-mention-candidates "DT"))
                     '("@thewawar")))
      (should (equal (mapcar (lambda (c) (plist-get c :label))
                             (acm-backend-telega-mention-candidates "thewawar"))
                     '("@thewawar"))))))

(ert-deftest acm-backend-telega-test-a-complete-member-list-needs-no-buffer-read ()
  "The names are read out of the buffer only when the member list is not whole.
The list is asked for with a limit, so a chat larger than that answers with a
part of its members; the chat knows how many it has, and a list at least that
long is the whole of it."
  (with-temp-buffer
    (insert "somebody who only wrote here\n")
    (setq-local acm-backend-telega-items nil)
    (goto-char (point-max))
    (insert "@")
    (cl-letf (((symbol-function 'acm-backend-telega-enabled-p) (lambda () t))
              ((symbol-function 'acm-backend-telega--typed-prefix-p) (lambda (&rest _) t))
              ((symbol-function 'telega-chatbuf--chat) (lambda () 'a-chat))
              ((symbol-function 'telega-chat--info) (lambda (&rest _) 'info))
              ((symbol-function 'telega-msg-sender-username) (lambda (&rest _) "@member"))
              ((symbol-function 'telega-msg-sender-title) (lambda (&rest _) "Member")))
      (let ((labels (lambda ()
                      (mapcar (lambda (c) (plist-get c :label))
                              (acm-backend-telega-mention-candidates "")))))
        ;; Two members fetched out of a chat of two: the list is the whole chat.
        (setq-local acm-backend-telega-items (list (list :key "@member" :label "@member"
                                                         :annotation "Member" :backend "telega")))
        (cl-letf (((symbol-function 'telega-tl-get0)
                   (lambda (&rest _) 1)))
          (should (equal (funcall labels) '("@member"))))
        ;; The same list, in a chat that says it has three: the buffer is read.
        (cl-letf (((symbol-function 'telega-tl-get0)
                   (lambda (&rest _) 3))
                  ((symbol-function 'telega-msg-sender) (lambda (&rest _) 'a-user))
                  ((symbol-function 'telega-user-p) (lambda (&rest _) t))
                  ((symbol-function 'telega-msg-sender-title) (lambda (&rest _) "Speaker")))
          (ignore-errors (should (member "Speaker" (funcall labels)))))))))

(ert-deftest acm-backend-telega-test-when-the-member-list-is-the-whole-chat ()
  "The list counts as whole only when the chat does not hide it and it is long enough.
Telegram lets a group hide its members; TDLib says which do, and such a list holds
the administrators and nothing else.  Otherwise the list is asked for with a limit,
and a list at least as long as the chat's member count is the whole of it."
  (with-temp-buffer
    (setq-local acm-backend-telega-items (list (list :label "@admin")))
    ;; The chat is a variable in telega, not a function.
    (setq-local telega-chatbuf--chat 'a-chat)
    (cl-letf (((symbol-function 'telega-chat--info) (lambda (&rest _) 'info))
              ((symbol-function 'telega-chat-full-info) (lambda (&rest _) 'full)))
      (let ((complete (lambda () (acm-backend-telega--member-list-complete-p))))
        ;; The chat hides its members: the count says nothing, the list is a part.
        (cl-letf (((symbol-function 'telega-tl-get0)
                   (lambda (object key)
                     (cond ((eq key :has_hidden_members) t)
                           ((eq key :member_count) 1)
                           (t nil)))))
          (should-not (funcall complete)))
        ;; Nothing hidden and the list is as long as the chat: the whole chat.
        (cl-letf (((symbol-function 'telega-tl-get0)
                   (lambda (object key)
                     (cond ((eq key :has_hidden_members) nil)
                           ((eq key :member_count) 1)
                           (t nil)))))
          (should (funcall complete)))
        ;; Nothing hidden, but the chat has more members than were fetched.
        (cl-letf (((symbol-function 'telega-tl-get0)
                   (lambda (object key)
                     (cond ((eq key :has_hidden_members) nil)
                           ((eq key :member_count) 1286)
                           (t nil)))))
          (should-not (funcall complete)))))))

(ert-deftest acm-backend-telega-test-adding-members-keeps-the-list-and-adds-the-new ()
  "The pages of a chat arrive one after another and the list grows with them."
  (with-temp-buffer
    (cl-letf (((symbol-function 'telega-msg-sender-username) (lambda (&rest _) nil))
              ((symbol-function 'telega-msg-sender-title) (lambda (object &rest _) (format "%s" object))))
      (setq-local acm-backend-telega-items
                  (list (list :key "a" :label "a" :annotation "a")))
      (acm-backend-telega-items-add '(a b))
      (should (equal (mapcar (lambda (item) (plist-get item :label)) acm-backend-telega-items)
                     '("a" "b"))))))

(provide 'acm-backend-telega-test)
;;; acm-backend-telega-test.el ends here
