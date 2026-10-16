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

(defun acm-backend-telega--mention-candidate (user)
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
    (message "Fetch telega userlist... done.")))

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
        (typed-end (point)))
    (save-excursion
      ;; The buttons telega made, one per person it has rendered.
      (let ((position (point-min)))
        (while (setq position (next-single-property-change position :telega-link))
          (when-let* ((link (get-text-property position :telega-link))
                      (sender (and (eq (car-safe link) 'sender) (cdr link)))
                      (candidate (ignore-errors
                                   (acm-backend-telega--mention-candidate sender)))
                      (label (plist-get candidate :label))
                      ((not (member label seen))))
            (push label seen)
            (push candidate candidates))))
      ;; And the usernames written in the text, which carry no sender object.
      (goto-char (point-min))
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
                          :backend "telega")
                    candidates)))))
    (nreverse candidates)))

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
        (let* ((from-buffer (acm-backend-telega--buffer-mention-candidates))
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
                    (string-match keyword (plist-get item :annotation))))
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
PREVIEW non-nil shows the candidate in an overlay instead of inserting it."
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
            #'acm-backend-telega--chat-buffer-p))

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
