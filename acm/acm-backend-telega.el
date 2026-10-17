;;; acm-backend-telega.el --- telega backend for acm -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;
;; Completion inside telega chat buffers: the members of the chat after "@",
;; emoji after a lone ":", the bot commands and quick replies after "/", a
;; language name inside a ``` fence, hashtags after "#", and Telegram's own
;; emoji search after ":" when the keyword closes with ":".
;;
;; The file stays uncompiled on purpose (`no-byte-compile'), because it uses
;; telega's macros - `with-telega-chatbuf' for one - that are neither required
;; nor declared here: compiled without telega on the load path they would turn
;; into function calls.
;;
;; It also relaxes two of lsp-bridge's completion predicates for telega chats,
;; because without that the ":" and "```" triggers can never fire there: one
;; keeps the popup alive right after ":", the other allows completion inside a
;; string, which is what a code fence looks like to a syntax table.
;;; Code:

(require 'cl-lib)

(defgroup acm-backend-telega nil
  "Telega backend for ACM."
  :group 'acm)

(defcustom acm-enable-telega t
  "If non-nil enable telega username completion."
  :type 'boolean
  :group 'acm-backend-telega)

(defcustom acm-backend-telega-member-fetch-limit 200
  "How many members are asked for when a chat is entered.

Two hundred is what the request answers with anyway, and it is enough to type a
mention at once.  The members beyond it are paged in while Emacs is idle, by
`acm-backend-telega--fetch-all-members', so that the whole chat is in hand
without the first answer being waited on."
  :type 'integer
  :group 'lsp-bridge)

(defcustom acm-backend-telega-member-fetch-all-limit 2000
  "How many members the idle fetch will page in at most.

The pages are two hundred members each, so this is ten of them for the default.
A chat larger than this keeps the first page and the ones up to it, and the
mention completion reads the buffer for the rest; a chat of a few hundred is
fetched whole."
  :type 'integer
  :group 'lsp-bridge)

(defcustom acm-backend-telega-member-fetch-idle-delay 2
  "Seconds of idleness before the rest of the members are fetched."
  :type 'number
  :group 'lsp-bridge)

(defcustom acm-backend-telega-quiet-member-list-error t
  "Do not show telega's message about members that cannot be listed.

A chat whose members are hidden answers the search for them with
\"Member list is inaccessible\", and the request is made for the mention
candidates whether or not it can be answered; the mention completion reads the
names the buffer has rendered for exactly that case, so the message reports
nothing that is wrong.  Every other message telega prints is left alone."
  :type 'boolean
  :group 'lsp-bridge)

(defun acm-backend-telega--keep-quiet-about-inaccessible-members (orig format &rest args)
  "Drop telega's own message about a member list that cannot be read.
The message is the one `telega-server.el' prints for a failed request, and it is
the request for the members of a chat whose members are hidden.  Only that
message is dropped; anything else, from telega or from anywhere else, is passed
on to ORIG with FORMAT and ARGS."
  (if (and acm-backend-telega-quiet-member-list-error
           (equal format "telega-server error: %s")
           (equal (plist-get (car args) :message) "Member list is inaccessible"))
      nil
    (apply orig format args)))

(advice-add 'message
            :around #'acm-backend-telega--keep-quiet-about-inaccessible-members)

(defcustom acm-backend-telega-buffer-scan-limit 200000
  "How many characters of the chat buffer the mention scan reads backwards.

The names are read out of the buffer the chat has already rendered, and a chat
of any length would otherwise be read whole on every keystroke.  Only this much
of the end of the buffer is read, which is where the messages last rendered
are; a name whose last message is older than that is still completed once the
member list has answered, and the member list is asked either way.

Two hundred thousand characters is about two milliseconds of reading, measured,
and covers the messages of a chat that has been left open for a long while; a
value small enough to miss the message someone is looking for is the one thing
to watch for here."
  :type 'integer
  :group 'lsp-bridge)

(defcustom acm-backend-telega-corfu-exclusive nil
  "If non-nil, corfu and lsp-bridge exclude each other in telega chats.
With this on, turning corfu on in a telega chat turns `lsp-bridge-mode' off
there, and turning corfu off turns it back on: the two completion frontends
should not be active in the same buffer.  It is off by default, because corfu
being on does not mean it completes anything in a chat."
  :type 'boolean
  :group 'acm-backend-telega)

(defvar-local acm-backend-telega-items 'fetch
  "List of loaded items.
Number if async request has been made, but request is not yet completed.
`fetch' if members has not yet been fetched.")

(defvar-local acm-backend-telega-hashtag-items nil
  "Hashtags the last `acm-backend-telega-hashtag-candidates' asked for.")

(defvar-local acm-backend-telega--fetch-all-timer nil
  "Timer that pages in the rest of this chat's members while Emacs is idle.")

(defvar-local acm-backend-telega-hashtag-timer nil
  "Timer of the pending hashtag search, or nil.")

(defvar-local acm-backend-telega-tg-emoji-items nil
  "Telegram emoji the last `acm-backend-telega-tg-emoji-candidates' asked for.")

(defvar-local acm-backend-telega-tg-emoji-timer nil
  "Timer of the pending Telegram emoji search, or nil.")

(defun acm-backend-telega-enabled-p ()
  "Return non-nil if lsp-bridge is enabled in telega chatbufs."
  (and lsp-bridge-mode acm-enable-telega (derived-mode-p 'telega-chat-mode)))

;;; Keeping corfu and lsp-bridge out of each other's way

(defun acm-backend-telega--corfu-toggle ()
  "Turn lsp-bridge off while corfu is on in a telega chat, and back on after.
Only telega chat buffers are touched.  The `lsp-bridge-mode' guard is what
makes this safe: `corfu-mode-hook' also runs when corfu is turned *off*, and
without the guard the two hooks would keep switching each other off forever.
The hook is installed whether or not `acm-backend-telega-corfu-exclusive' is
set, so that setting the option later still takes effect."
  (when (derived-mode-p 'telega-chat-mode)
    (if corfu-mode
        (when (bound-and-true-p lsp-bridge-mode)
          (lsp-bridge-mode -1))
      (unless (bound-and-true-p lsp-bridge-mode)
        (lsp-bridge-mode 1)))))

;; The option is checked inside the toggle rather than here, because a config
;; normally sets it *after* this file is loaded - with the hook installed only
;; when the option was already set, such a config would never get the behaviour.
(add-hook 'corfu-mode-hook #'acm-backend-telega--corfu-toggle)

;;; The candidates of every source

(defun acm-backend-telega--mention-candidate (user &optional rendered-name)
  "Return the completion candidate of the chat member USER.
The label is the username when USER has one - `telega-msg-sender-username'
asked `with-@' - and the member title otherwise; the title always becomes
the annotation, and the member itself is kept for the expansion, which
needs it to build a mention entity for a member without a username."
  (let* ((username (ignore-errors (telega-msg-sender-username user 'with-@)))
         (title (ignore-errors
                  (telega-msg-sender-title user
                    :with-avatar-p nil
                    :with-badges-p nil
                    :with-title-faces-p nil)))
         (label (or username title)))
    (list :key (or username title)
          :icon "at"
          :label label
          :displayLabel label
:annotation title
          ;; What the chat shows for this person, which is what someone typing their
          ;; name has in mind: the title, the username, and, for a name read out of
          ;; the buffer, the text of the button it was read from.
          :search (mapconcat #'identity (delq nil (list rendered-name title username)) " ")
          :member user
          :backend "telega")))

(defun acm-backend-telega--emoji-candidate (entry)
  "Return the completion candidate of the emoji ENTRY.
ENTRY is a `(name . char)' pair of `telega-emoji-alist'."
  (list :key (car entry)
        :icon "emoji"
        :label (car entry)
        :displayLabel (car entry)
        :annotation (cdr entry)
        :emoji t
        :emoji-char (cdr entry)
        :backend "telega"))

(defun acm-backend-telega--precode-candidate (lang)
  "Return the completion candidate of the code fence language LANG."
  (list :key lang
        :icon "code"
        :label lang
        :displayLabel lang
        :annotation "code block"
        :precode t
        :backend "telega"))

(defun acm-backend-telega--hashtag-candidate (hashtag)
  "Return the completion candidate of the HASHTAG, without its \"#\"."
  (list :key (concat "#" hashtag)
        :icon "tag"
        :label (concat "#" hashtag)
        :displayLabel (concat "#" hashtag)
        :annotation "hashtag"
        :hashtag t
        :backend "telega"))

(defun acm-backend-telega--tg-emoji-candidate (emoji-keyword)
  "Return the completion candidate of the Telegram EMOJI-KEYWORD.
EMOJI-KEYWORD is one entry of the `:emojis' of `telega--searchEmojis'."
  (let ((name (telega-tl-str emoji-keyword :keyword))
        (emoji (telega-tl-str emoji-keyword :emoji)))
    (list :key (concat ":" name ":")
          :icon "emoji"
          :label (concat ":" name ":")
          :displayLabel (concat ":" name ":")
          :annotation emoji
          :emoji t
          :emoji-char emoji
          :backend "telega")))

;;; The members of the chat

(defun acm-backend-telega-items-set (members)
  "Set `acm-backend-telega-items' from MEMBERS in the current chatbuf.
It is the callback of `telega--searchChatMembers'.  Every member becomes a
candidate: those with a username are labelled with it, the others with their
title, and the expansion turns the latter into a mention entity."
  ;; The request is asynchronous, so the buffer may have left the chat (or the
  ;; mode may have been turned off) while it was in flight.  That is not an
  ;; error worth aborting on: there is simply nothing to update any more.
  (when (acm-backend-telega-enabled-p)
    (setq acm-backend-telega-items
          (mapcar #'acm-backend-telega--mention-candidate members))
    (message "Fetch telega userlist... done.")
    ;; The first answer is one page of the members.  The rest are paged in while
    ;; Emacs is idle, so that typing a mention does not wait for a chat of a few
    ;; hundred members to be fetched whole.
    (let ((chat telega-chatbuf--chat))
      (when (timerp acm-backend-telega--fetch-all-timer)
        (cancel-timer acm-backend-telega--fetch-all-timer))
      (setq acm-backend-telega--fetch-all-timer
            (run-with-idle-timer acm-backend-telega-member-fetch-idle-delay nil
                                 (lambda ()
                                   (setq acm-backend-telega--fetch-all-timer nil)
                                   (acm-backend-telega--fetch-all-members chat 0)))))))

(defun acm-backend-telega-items-add (members)
  "Add MEMBERS to `acm-backend-telega-items', keeping the ones already there.

The first answer of a chat is a page of its members and the rest arrive later,
so the list grows instead of being replaced: a member whose label is already in
it is not added twice."
  (when (and members (listp acm-backend-telega-items))
    (let ((known (mapcar (lambda (item) (plist-get item :label))
                         acm-backend-telega-items)))
      (setq acm-backend-telega-items
            (append acm-backend-telega-items
                    (seq-remove (lambda (item) (member (plist-get item :label) known))
                                (mapcar #'acm-backend-telega--mention-candidate members)))))))

(defun acm-backend-telega--fetch-all-members (chat offset)
  "Page in the members of CHAT from OFFSET, a page at a time.

The members of a chat are asked for two hundred at a time, and the idle fetch
walks the pages so that the whole chat is in hand; it stops when a page comes
back short, when the chat's member count is reached, or at
`acm-backend-telega-member-fetch-all-limit'.  Chats that are not supergroups have
no pages to walk: their members come with the first answer."
  (when-let* ((_ (telega-chat-match-p chat '(type supergroup)))
              (_ (< offset acm-backend-telega-member-fetch-all-limit)))
    (telega--getSupergroupMembers
        chat nil offset acm-backend-telega-member-fetch-limit
      (lambda (members)
        (with-telega-chatbuf chat
          (acm-backend-telega-items-add members)
          (when (and members
                     (= (length members) acm-backend-telega-member-fetch-limit)
                     (< (+ offset acm-backend-telega-member-fetch-limit)
                        acm-backend-telega-member-fetch-all-limit))
            (acm-backend-telega--fetch-all-members
             chat (+ offset acm-backend-telega-member-fetch-limit))))))))

(defun acm-backend-telega-update-items (&rest _args)
  "Update the optional items."
  ;; Scoped by current line: the @ character must be present before the cursor
  (when (acm-backend-telega-enabled-p)
    (cond ((version< telega-version "0.8.162")
           (message "telega: Update your telega to support lsp-bridge"))
          ((eq acm-backend-telega-items 'fetch)
           (let ((chat telega-chatbuf--chat))
             (cl-assert chat)
             (setq acm-backend-telega-items
                   ;; NOTE: fetching members for channels is available
                   ;; only for admins (from TDLib docs)
                   (when (telega-chat-match-p chat '(or (not (type channel))
                                                        (me-is-owner or-admin)))
                     (message "Fetch telega userlist...")
                     ;; TODO: possibly use "chatMembersFilterMention"
                     ;; member filter, because chat might be filtering
                     ;; messages by topic, see
                     ;; `telega-company-username' for details
                     (telega--searchChatMembers chat "" nil
                       :limit acm-backend-telega-member-fetch-limit
                       :callback
                       (lambda (members)
                         (with-telega-chatbuf chat
                           (acm-backend-telega-items-set members)))))))))))

(defun acm-backend-telega--typed-prefix-p (keyword char)
  "Non-nil when KEYWORD was typed right after CHAR.
`acm-input-bound-style' decides whether the trigger character is part of the
prefix: with the string style the prefix starts after the last whitespace, so
the trigger arrives inside KEYWORD, while the symbol and ascii styles hand over
the symbol at point and it arrives alone.  Both have to be accepted, or every
source that looks for the trigger character is blind to one of them - which is
what made only the slash command source complete."
  (or (and (> (length keyword) 0) (= (aref keyword 0) char))
      (save-excursion
        (backward-char (length keyword))
        (= (char-before) char))))


(defun acm-backend-telega--buffer-mention-candidates ()
  "Return mention candidates for the people already rendered in this buffer.

Telega wraps every name it renders - a message sender, a voter, an
administrator - in a button whose `:telega-link' property holds the sender
object itself, and that object is there whether or not the member has a
username.  Reading those buttons back gives the people this buffer has shown,
which is what completes while the member list cannot be fetched at all: a chat
whose members are hidden answers `searchChatMembers' with nothing, and these
names are the ones left.

A bare `@username' written in the text is offered as well, because a name that
is only mentioned in a message is not rendered as a button of its own."
  (let ((candidates nil) (seen nil)
        ;; Where the user is typing: the occurrence that ends here is the prefix
        ;; being completed, not a name written in the chat.
        (typed-end (point))
        ;; Only the end of the buffer is read, so a long chat costs what its last
        ;; messages cost however much history it holds.
        (from (max (point-min) (- (point-max) acm-backend-telega-buffer-scan-limit))))
    (save-excursion
      ;; The buttons telega made, one per person it has rendered.
      (goto-char from)
      (let ((position from))
        (while (setq position (next-single-property-change position :telega-link))
          (when-let* ((link (get-text-property position :telega-link))
                      (sender (and (eq (car-safe link) 'sender) (cdr link)))
                      ;; The name as the chat renders it: the text of the button.
                      (rendered (string-trim
                                 (buffer-substring-no-properties
                                  position (or (next-single-property-change
                                                position :telega-link)
                                               (point-max)))))
                      (candidate (ignore-errors
                                   (acm-backend-telega--mention-candidate sender rendered)))
                      (label (plist-get candidate :label))
                      ((not (member label seen))))
            (push label seen)
            (push candidate candidates))))
      ;; And the sender of every message the buffer has rendered.  Telega puts the
      ;; message object itself in the `:value' property of the text it wrote for it,
      ;; and the sender is one lookup away from it: this is what finds the people who
      ;; have spoken in the chat, which is everyone the buffer shows, whether or not
      ;; the member list can be read and whether or not they have a username.
      (goto-char from)
      (let ((at from))
        (while (and (< at (point-max))
                    (setq at (next-single-property-change at :value)))
          (let* ((message (get-text-property at :value))
                 (sender (and (listp message)
                              (plist-get message :sender_id)
                              (ignore-errors
                                (telega-msg-sender (plist-get message :sender_id))))))
            (when (and sender (ignore-errors (telega-user-p sender)))
              (when-let* ((candidate (ignore-errors
                                       (acm-backend-telega--mention-candidate sender)))
                          (label (plist-get candidate :label))
                          ((not (member label seen))))
                (push label seen)
                (push candidate candidates))))))
      ;; And the usernames written in the text, which carry no sender object.
      (goto-char from)
      (while (re-search-forward "@\\([[:alnum:]_]+\\)" nil t)
        (let ((label (match-string 0)))
          (unless (or (member label seen)
                      (= (match-end 0) typed-end))
            (push label seen)
            (push (list :key label
                        :icon "at"
                        :label label
                        :displayLabel label
                        :annotation "mentioned in this chat"
                        ;; What can be typed to find them: the name in the text.
                        :search label
                        :backend "telega")
                    candidates)))))
    (nreverse candidates)))

(defun acm-backend-telega--buffer-mention-candidates-safely ()
  "Return `acm-backend-telega--buffer-mention-candidates', or nil on any error.

The names are read out of a buffer that telega owns and that this function only
knows the shape of; when it is not that shape, or when reading it fails for any
other reason, the answer is nothing rather than an error - a mention scan that
raised would take the emoji, the hashtags and the slash commands down with it,
because the mention candidates are asked for first."
  (ignore-errors (acm-backend-telega--buffer-mention-candidates)))

(defun acm-backend-telega--member-list-complete-p ()
  "Return non-nil when the fetched member list holds every member of the chat.

Two things make a list a part of the chat.  Telegram lets a group hide its members,
and TDLib says which groups do in `has_hidden_members'; the list of such a chat is
its administrators and nothing else, which is the chat the read exists for.  And
the list is asked for with a limit, so a chat larger than that limit answers with a
part of its members without saying so - while the chat knows how many it has.  When
neither is known, the answer is nil and the buffer is read, which is what finds the
people a partial list cannot show.

Reading the buffer is what finds the people the list cannot show, so skipping it
is only safe when the list is known to be complete."
  (let* ((items acm-backend-telega-items)
         (chat (and (boundp 'telega-chatbuf--chat) telega-chatbuf--chat))
         (full-info (and chat (ignore-errors (telega-chat-full-info chat))))
         ;; Telegram lets a group hide its member list, and TDLib says so here:
         ;; the list then holds the administrators and nothing else.
         (hidden (and full-info
                      (ignore-errors
                        (telega-tl-get0 full-info :has_hidden_members))))
         (info (and chat (ignore-errors (telega-chat--info chat))))
         (total (and info (ignore-errors (telega-tl-get0 info :member_count)))))
    (and (listp items)
         items
         chat
         (not (eq hidden t))
         (let ((total (if (integerp total)
                          total
                        (ignore-errors (telega-tl-get0 full-info :member_count)))))
           (and (integerp total)
                (> total 0)
                (>= (length items) total))))))

(defun acm-backend-telega-mention-candidates (keyword)
  "Return the chat member candidates matching KEYWORD.
Return nil unless lsp-bridge is enabled in the current telega chat buffer
and \"@\" precedes KEYWORD.  When KEYWORD is empty return
`acm-backend-telega-items' unchanged, otherwise return the items whose label
or annotation matches KEYWORD, sorted by prefix."
  (when (and (acm-backend-telega-enabled-p)
             (acm-backend-telega--typed-prefix-p keyword ?@))
    ;; The member list is fetched once per chat, asynchronously.
    ;; `telega-chatbuf--switch-in' normally asks for it, but that hook can run
    ;; before lsp-bridge-mode is on in the buffer (or with the mode off), and then
    ;; nothing would ever ask again: the sentinel would stay `fetch' and "@"
    ;; would never complete.  Ask here too, so the first keystroke after entering
    ;; a chat starts the fetch instead of relying on that one hook.
    (when (eq acm-backend-telega-items 'fetch)
      (acm-backend-telega-update-items))
        ;; What the buffer has rendered is offered beside what the member list
        ;; answered, so a chat whose members cannot be fetched still completes.  A name
        ;; that comes from both is kept once, the member list's copy first.
        (let* ((from-buffer (unless (acm-backend-telega--member-list-complete-p)
                              (acm-backend-telega--buffer-mention-candidates-safely)))
               (fetched (and (listp acm-backend-telega-items) acm-backend-telega-items))
               (labels (mapcar (lambda (item) (plist-get item :label)) fetched))
               (extra (seq-remove (lambda (item)
                                    (member (plist-get item :label) labels))
                                  from-buffer))
               (items (append fetched extra)))
          (if (string-equal keyword "")
              items
            (acm-candidate-sort-by-prefix
             keyword
             (seq-filter
              (lambda (item)
                (or (string-match keyword (plist-get item :label))
                   ;; The label is the username, the annotation the title and
                   ;; `:search' what the chat rendered; the name typed may be any of
                   ;; the three, so all three are asked.
                   (string-match keyword (plist-get item :annotation))
                   (string-match keyword (or (plist-get item :search) ""))))
              items))))))

;;; Emoji: :<name> -> the emoji character

(defun acm-backend-telega-emoji-candidates (keyword)
  "Return the emoji candidates matching KEYWORD.
Only in a telega chat buffer and only when a lone \":\" precedes KEYWORD,
which is what tells an emoji keyword from any other completion."
  (when (and (derived-mode-p 'telega-chat-mode)
             (acm-backend-telega--typed-prefix-p keyword ?:))
    (telega-emoji-init)
    (let ((prefix (concat ":" keyword)))
      (mapcar #'acm-backend-telega--emoji-candidate
              (cl-remove-if-not
               (lambda (entry) (string-prefix-p prefix (car entry)))
               telega-emoji-alist)))))

;;; Slash: /<command> -> the bot commands and the quick replies

(defun acm-backend-telega-slash-candidates (keyword)
  "Return the bot command and quick reply candidates matching KEYWORD.
\"/\" is a word constituent, so KEYWORD already carries it and the source is
recognised by that prefix rather than by the character before point."
  (when (and (derived-mode-p 'telega-chat-mode)
             (string-prefix-p "/" keyword))
    (let ((prefix keyword)
          result)
      (dolist (cmd (ignore-errors
                     (telega-completions--bot-commands telega-chatbuf--chat)))
        (when (string-prefix-p prefix cmd)
          (push (list :key cmd
                      :icon "robot"
                      :label cmd
                      :displayLabel cmd
                      :annotation (get-text-property 0 'telega-annotation cmd)
                      :slash t
                      :backend "telega")
                result)))
      (when (ignore-errors (telega-chatbuf-match-p '(type private)))
        (dolist (qr (ignore-errors (telega-completions--quick-replies)))
          (when (string-prefix-p prefix qr)
            (push (list :key qr
                        :icon "reply"
                        :label qr
                        :displayLabel qr
                        :annotation (get-text-property 0 'telega-annotation qr)
                        :slash t
                        :backend "telega")
                  result))))
      result)))

;;; Precode: ```<lang> -> the language names of the major modes

(defun acm-backend-telega-precode-candidates (keyword)
  "Return the code fence language candidates matching KEYWORD.
The keyword is completed inside the fence on the current line, so the
position where the completion started is compared with the match of the
fence rather than with the word before point."
  (when (derived-mode-p 'telega-chat-mode)
    ;; `acm-input-bound-style' decides whether the fence belongs to the prefix:
    ;; the string style has no whitespace to stop at before the backticks, so
    ;; KEYWORD arrives as the fence plus what was typed, while the symbol styles
    ;; hand over the language alone.  Strip it, since the languages are compared
    ;; without it - and the expansion puts it back when the prefix includes it.
    (let ((keyword (string-remove-prefix "```" keyword))
          (orig-point (point)))
      (save-excursion
        (beginning-of-line)
        (when (looking-at "```\\([^`\t\n ]*\\)")
          (let ((lang-start (match-beginning 1))
                (lang-end (match-end 1)))
            (when (and (>= orig-point lang-start)
                       (<= orig-point lang-end))
              (mapcar #'acm-backend-telega--precode-candidate
                      (cl-remove-if-not
                       (lambda (lang) (string-prefix-p keyword lang))
                       (ignore-errors (telega-completions--language-names)))))))))))

;;; Hashtag: #<tag> through TDLib searchHashtags

(defun acm-backend-telega-hashtag-candidates (keyword)
  "Return the hashtag candidates matching KEYWORD.
TDLib answers asynchronously, so the search is debounced with
`acm-backend-telega-hashtag-timer' and the popup is refreshed through
`acm-update' when the answer arrives; the previous results are returned in
the meantime, with a placeholder while nothing has been searched yet."
  (when (and (derived-mode-p 'telega-chat-mode)
             (acm-backend-telega--typed-prefix-p keyword ?#))
    (when acm-backend-telega-hashtag-timer
      (cancel-timer acm-backend-telega-hashtag-timer))
    (let ((query keyword)
          (buf (current-buffer)))
      (setq acm-backend-telega-hashtag-timer
            (run-with-timer 0.2 nil
                            (lambda ()
                              (telega--searchHashtags query
                                :callback
                                (lambda (hashtags)
                                  (with-current-buffer buf
                                    (setq acm-backend-telega-hashtag-items
                                          (mapcar #'acm-backend-telega--hashtag-candidate
                                                  hashtags))
                                    (acm-update)))))))
      (or acm-backend-telega-hashtag-items
          (list (list :key "#..."
                      :icon "tag"
                      :label "#..."
                      :displayLabel "#..."
                      :annotation "searching..."
                      :backend "telega"))))))

;;; Telegram emoji: :<name>: through TDLib searchEmojis

(defun acm-backend-telega-tg-emoji-candidates (keyword)
  "Return the Telegram emoji candidates matching KEYWORD.
TDLib answers asynchronously, so the search is debounced with
`acm-backend-telega-tg-emoji-timer' and the popup is refreshed through
`acm-update' when the answer arrives; the search replaces \"-\" with a space
because that is how TDLib splits the words of an emoji keyword."
  (when (and (derived-mode-p 'telega-chat-mode)
             (> (length keyword) 0)
             (acm-backend-telega--typed-prefix-p keyword ?:))
    (let ((search-text (replace-regexp-in-string "-" " " keyword))
          (buf (current-buffer)))
      (when acm-backend-telega-tg-emoji-timer
        (cancel-timer acm-backend-telega-tg-emoji-timer))
      (setq acm-backend-telega-tg-emoji-timer
            (run-with-timer 0.2 nil
                            (lambda ()
                              (telega--searchEmojis search-text
                                :callback
                                (lambda (emoji-keywords)
                                  (with-current-buffer buf
                                    (setq acm-backend-telega-tg-emoji-items
                                          (mapcar #'acm-backend-telega--tg-emoji-candidate
                                                  emoji-keywords))
                                    (acm-update)))))))
      acm-backend-telega-tg-emoji-items)))

;;; The backend entry points

(defun acm-backend-telega-candidates (keyword)
  "Return every telega candidate matching KEYWORD.
The sources are asked in turn: the chat members, then the emoji, the
Telegram emoji, the bot commands and quick replies, the hashtags and the
code fence languages.  Each one decides for itself whether it applies, so
this is a plain append of whatever they answer."
  (append (acm-backend-telega-mention-candidates keyword)
          (acm-backend-telega-emoji-candidates keyword)
          (acm-backend-telega-tg-emoji-candidates keyword)
          (acm-backend-telega-slash-candidates keyword)
          (acm-backend-telega-hashtag-candidates keyword)
          (acm-backend-telega-precode-candidates keyword)))

(defun acm-backend-telega-candidate-expand (candidate-info bound-start &optional preview)
  "Insert the telega candidate CANDIDATE-INFO, replacing BOUND-START onwards.
Every source replaces the token at point, and the leading character of that
token - \"@\", \"/\", \"#\", \":\" or the fence backtick - is part of the
bound to delete.  A mention of a member without a username becomes a mention
entity carrying the member id, which is the only way Telegram accepts it.
Telega marks the messages of a chat buffer read-only, and the token being
completed is written over when a candidate is taken, so the write happens
with `inhibit-read-only' bound: without it, taking the one candidate of a
mention - which lsp-bridge does by itself when there is only one - signalled
`text-read-only' from the post-command hook.
PREVIEW non-nil shows the candidate in an overlay instead of inserting it."
  (let ((inhibit-read-only t))
    (cond
     ;; Emoji, slash, precode and hashtag candidates all replace the token at point.
     ((plist-get candidate-info :emoji)
      (let ((text (plist-get candidate-info :emoji-char)))
        (when (eq (char-before bound-start) ?:)
          (setq bound-start (1- bound-start)))
        (if preview
            (acm-preview-create-overlay bound-start (point) text)
          (delete-region bound-start (point))
          (insert text))))
     ((plist-get candidate-info :slash)
      (let ((text (plist-get candidate-info :label)))
        (when (eq (char-before bound-start) ?/)
          (setq bound-start (1- bound-start)))
        (if preview
            (acm-preview-create-overlay bound-start (point) text)
          (delete-region bound-start (point))
          (insert text)
          (insert " "))))
     ((plist-get candidate-info :precode)
      (let* ((text (plist-get candidate-info :label))
             ;; `bound-start' begins the prefix.  With the string style that is the
             ;; first backtick, so the fence is inside the replaced region and has to
             ;; be inserted again; with the symbol styles it sits right after the
             ;; fence, which the buffer already has.
             (text (if (eq (char-before bound-start) ?`)
                       text
                     (concat "```" text))))
        (if preview
            (acm-preview-create-overlay bound-start (point) text)
          (delete-region bound-start (point))
          (insert text)
          ;; Mirror telega: insert closing ``` on a new line, leave cursor between.
          (insert "\n\n```")
          (forward-line -1)
          (beginning-of-line))))
     ((plist-get candidate-info :hashtag)
      (let ((text (plist-get candidate-info :label)))
        (when (eq (char-before bound-start) ?#)
          (setq bound-start (1- bound-start)))
        (if preview
            (acm-preview-create-overlay bound-start (point) text)
          (delete-region bound-start (point))
          (insert text)
          (insert " "))))
     ;; Mention candidate: insert @username, or a mention entity when the member
     ;; has no username (which is also why the member is kept in the candidate).
        ;; A name read out of the buffer's text is inserted as it stands: there is no
        ;; sender object behind it, and its label is the `@username' the text holds.
        ((and (null (plist-get candidate-info :member))
              (plist-get candidate-info :label))
         (let ((text (plist-get candidate-info :label)))
           (if preview
               (acm-preview-create-overlay bound-start (point) text)
             (delete-region bound-start (point))
             (insert text)
             (insert " "))))
     (t
      (let* ((member (plist-get candidate-info :member))
             (username (ignore-errors (telega-msg-sender-username member 'with-@)))
             (title (ignore-errors
                      (telega-msg-sender-title member
                        :with-avatar-p nil
                        :with-badges-p nil
                        :with-title-faces-p nil)))
             (label (or username title)))
        ;; Delete the leading @ prefix (inserted label starts with @ for
        ;; username users, or is a mention entity for no-username users)
        (when (eq (char-before bound-start) ?@)
          (setq bound-start (1- bound-start)))
        (if preview
            (acm-preview-create-overlay bound-start (point) label)
          (delete-region bound-start (point))
          (if username
              (insert username)
            (let ((start (point)))
              (insert title)
              (put-text-property start (point) :tl-entity-type
                                 (list :@type "textEntityTypeMentionName"
                                       :user_id (plist-get member :id)))
              (put-text-property start (point) 'face 'telega-entity-type-mention)))
          ;; Append a space right after the mention, mirroring
          ;; `telega-completions--username-post-completion' behaviour.
          (insert " "))))))

  ;;; Keeping telega's own triggers reachable

  (with-eval-after-load 'lsp-bridge
    ;; The two questions a telega chat answers differently from the syntax table
    ;; are answered through the hooks lsp-bridge offers for them.
    (add-hook 'lsp-bridge-completion-hide-character-functions
              #'acm-backend-telega--colon-visible-p)
    (add-hook 'lsp-bridge-completion-in-string-functions
              #'acm-backend-telega--chat-buffer-p)))

;; Update userlist when first switch to telega buffer.
(advice-add 'telega-chatbuf--switch-in
            :after #'acm-backend-telega-update-items)

(defun acm-backend-telega--colon-visible-p ()
  "Return non-nil in a telega chat, where a colon starts an emoji keyword.
lsp-bridge hides the menu right after \":\" by default; here it may not."
  (and (derived-mode-p 'telega-chat-mode)
       (eq (char-before) ?:)))

(defun acm-backend-telega--chat-buffer-p ()
  "Return non-nil in a telega chat, where a code fence is completed in a string."
  (derived-mode-p 'telega-chat-mode))

(provide 'acm-backend-telega)
;;; acm-backend-telega.el ends here
