;;; acm-backend-devonthink.el --- DEVONthink completion backend  -*- lexical-binding: t; -*-

;; Filename: acm-backend-devonthink.el
;; Description: Complete DEVONthink record names and insert them as org links.
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2026, Andy Stewart, all rights reserved.
;; Created: 2026-10-11 07:55:00
;; Version: 0.1
;; Last-Updated: 2026-10-11 07:55:00
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/acm-backend-devonthink
;; Keywords: completion, devonthink
;; Compatibility: GNU Emacs 30.0.50
;;
;; Features that might be required by this library:
;;
;;   `acm', `cl-lib', `subr-x'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; DEVONthink keeps its databases to itself: there is no query API and the
;; database files are not meant to be read from the outside.  What it does offer
;; is AppleScript, and every record carries a UUID that its `x-devonthink-item'
;; URL is built from.  This backend uses those two facts and nothing else.
;;
;; An AppleScript round trip takes tenths of a second, which completion cannot
;; wait for, so a search is debounced, answered by a background process, and
;; cached per search string; the candidates known so far are returned in the
;; meantime, and the answer refreshes the menu through `acm-update'.
;;
;; Type `dt:' followed by part of a record name to search.  Picking a candidate
;; inserts it as an org link, [[x-devonthink-item://UUID][Name]], which is what
;; the reference implementation org-mac-link inserts as well.  The trigger ends
;; in ":", and ":" is in `lsp-bridge-completion-hide-characters', so the advice
;; at the end of this file keeps the popup alive right after it - narrowly, only
;; while the text before point is the trigger.

;;; Installation:
;;
;; Put acm-backend-devonthink.el to your load-path, then:
;;
;; (require 'acm-backend-devonthink)
;; (setq acm-enable-devonthink t)

;;; Customize:
;;
;; `acm-enable-devonthink' is off by default, so nothing runs until it is set.
;; `acm-backend-devonthink-application-id' is the AppleScript id the search
;; addresses, "DNtp", which did not change when the application was renamed.
;;
;; All of the above can customize by:
;;      M-x customize-group RET acm-backend-devonthink RET

;;; Change log:
;;
;; 2026/10/11
;;      * First released.

;;; Acknowledgements:
;;
;; org-mac-link (https://gitlab.com/aimebertrand/org-mac-link) for the AppleScript
;; way of reaching DEVONthink records and for the org link form.

;;; TODO
;;
;; Searching one database only: the `search' command takes a group to search in,
;; and a database option would have to be resolved to that group first.

;;; Require

(require 'cl-lib)
(require 'subr-x)

(declare-function lsp-bridge--not-match-hide-characters "lsp-bridge-completion")
(declare-function acm-preview-create-overlay "acm")
(declare-function acm-update "acm")

;;; Code:

(defcustom acm-enable-devonthink nil
  "Enable the DEVONthink completion backend.
Nothing is asked of DEVONthink while this is nil."
  :type 'boolean
  :group 'acm-backend-devonthink)

(defcustom acm-backend-devonthink-trigger "dt:"
  "Text that has to be typed before a DEVONthink search starts."
  :type 'string
  :group 'acm-backend-devonthink)

(defcustom acm-backend-devonthink-application-id "DNtp"
  "AppleScript id of the DEVONthink application to ask.
The application has been renamed over the years - DEVONthink Pro,
DEVONthink 3, DEVONthink - while this id stayed the same, which is why
the search addresses the application by id instead of by name.  The
bundle identifier the system reports, com.devon-technologies.think, is
a different thing and DEVONthink does not answer to it here."
  :type 'string
  :group 'acm-backend-devonthink)

(defcustom acm-backend-devonthink-limit 30
  "Maximum number of records to ask DEVONthink for."
  :type 'integer
  :group 'acm-backend-devonthink)

(defcustom acm-backend-devonthink-debounce 0.25
  "Seconds to wait after the last keystroke before searching."
  :type 'number
  :group 'acm-backend-devonthink)

(defcustom acm-backend-devonthink-cache-seconds 60
  "How long the answer for one search string stays fresh.
An AppleScript round trip is too slow to repeat on every keystroke, so answers
are cached; they are searched again once they are older than this."
  :type 'number
  :group 'acm-backend-devonthink)

(defcustom acm-backend-devonthink-url-scheme "x-devonthink-item"
  "URL scheme DEVONthink records are referenced by."
  :type 'string
  :group 'acm-backend-devonthink)

(defvar acm-backend-devonthink--results nil
  "Alist of SEARCH-STRING to (TIMESTAMP . CANDIDATES).
TIMESTAMP is when the answer arrived, and is what
`acm-backend-devonthink-cache-seconds' is compared against.")

(defvar acm-backend-devonthink--timer nil
  "Debounce timer of the search that is about to start.")

(defvar acm-backend-devonthink--pending nil
  "Search string of the query a process is answering right now.")

(defvar acm-backend-devonthink--process nil
  "AppleScript process of the query that is being answered.")

(defun acm-backend-devonthink--trigger-p (keyword)
  "Return non-nil when KEYWORD is a DEVONthink search.
`acm-input-bound-style' decides whether the trigger is part of the prefix: the
string style hands over \"dt:foo\", while the symbol and ascii styles hand over
\"foo\" and leave the trigger right before point.  Both are accepted, because
the sources are otherwise blind to one of the two."
  (let ((trigger acm-backend-devonthink-trigger)
        (start (- (point) (length keyword))))
    (or (string-prefix-p trigger keyword)
        ;; KEYWORD ends at point, so the trigger would be right before its start
        (and (>= start (length trigger))
             (equal keyword (buffer-substring-no-properties start (point)))
             (save-excursion
               (goto-char start)
               (backward-char (length trigger))
               (looking-at (regexp-quote trigger)))))))

(defun acm-backend-devonthink--query (keyword)
  "Return the search string that KEYWORD asks for, without the trigger."
  (string-remove-prefix acm-backend-devonthink-trigger keyword))

(defun acm-backend-devonthink--quote-applescript (string)
  "Return STRING ready to be embedded in an AppleScript string literal.
Backslashes and double quotes are the only characters that would end the
literal early."
  (replace-regexp-in-string "[\\\"]" (lambda (match) (concat "\\" match)) string t t))

(defun acm-backend-devonthink--script (query)
  "Return the AppleScript that searches the record names for QUERY.
The `name:' key and the trailing wildcard make DEVONthink do a prefix search on
the record names, which is what completion needs, and the loop stops at
`acm-backend-devonthink-limit' records.  Each record is written as
UUID, name and kind, separated by tabs - written as `(ASCII character 9)'
rather than the `tab' constant, which DEVONthink's own tell block answers with
the four letters of its name."
  (format "tell application id \"%s\"
  set theRecords to search \"name:%s*\"
  set theOutput to \"\"
  set theCount to 0
  repeat with theRecord in theRecords
    if theCount is greater than or equal to %d then exit repeat
    set theOutput to theOutput & (uuid of theRecord) & (ASCII character 9) & (name of theRecord) & (ASCII character 9) & (kind of theRecord) & linefeed
    set theCount to theCount + 1
  end repeat
  return theOutput
end tell"
          (acm-backend-devonthink--quote-applescript acm-backend-devonthink-application-id)
          (acm-backend-devonthink--quote-applescript query)
          acm-backend-devonthink-limit))

(defun acm-backend-devonthink--hint ()
  "Return the candidate shown when the trigger has just been typed.
There is nothing to search for yet, and showing this is what makes the menu
appear on the trigger itself rather than only after the next character."
  (list :key "acm-devonthink-hint"
        :icon "note"
        :label "Type to search DEVONthink"
        :displayLabel "Type to search DEVONthink"
        :annotation "dt:"
        :uuid nil
        :backend "devonthink"))

(defun acm-backend-devonthink--placeholder ()
  "Return the candidate that stands in while DEVONthink is being asked.
The answer needs an AppleScript round trip, and the menu has to appear on the
keystroke that starts the search, so this is shown until the answer arrives."
  (list :key "acm-devonthink-searching"
        :icon "note"
        :label "Searching DEVONthink..."
        :displayLabel "Searching DEVONthink..."
        :annotation "searching"
        :uuid nil
        :backend "devonthink"))

(defun acm-backend-devonthink--parse (output)
  "Turn the tab separated OUTPUT of a search into candidates."
  (delq nil
        (mapcar (lambda (line)
                  (let ((line (string-trim line)))
                    (when (string-match "\\`\\([^\t]+\\)\t\\([^\t]+\\)\t\\(.*\\)\\'" line)
                      (list :key (match-string 2 line)
                            :icon "note"
                            :label (match-string 2 line)
                            :displayLabel (match-string 2 line)
                            :annotation (match-string 3 line)
                            :uuid (match-string 1 line)
                            :backend "devonthink"))))
                (split-string output "\n" t))))

(defun acm-backend-devonthink--remember (query candidates)
  "Cache CANDIDATES as the answer for QUERY.
Only the most recent answers are kept, because each one is a search the user
typed."
  (setq acm-backend-devonthink--results
        (cons (cons query (cons (float-time) candidates))
              (cl-remove-if (lambda (cell) (equal (car cell) query))
                            acm-backend-devonthink--results)))
  (when (> (length acm-backend-devonthink--results) 20)
    (setq acm-backend-devonthink--results
          (cl-subseq acm-backend-devonthink--results 0 20))))

(defun acm-backend-devonthink--sentinel (buffer query process event)
  "Store what PROCESS answered for QUERY and refresh BUFFER.
EVENT is ignored unless the process finished, so that the answer of a process
that was replaced by a newer search is not mistaken for one."
  (when (string-prefix-p "finished" event)
    (unwind-protect
        (let* ((completion (with-current-buffer (process-buffer process)
                             (buffer-substring-no-properties (point-min) (point-max))))
               (failure (process-exit-status process)))
          (setq acm-backend-devonthink--pending nil)
          (if (/= failure 0)
              ;; osascript writes the reason - a refused automation permission,
              ;; for instance - to its error stream, and saying it out loud is
              ;; the only way to tell the user why nothing came back.
              (message "DEVONthink search failed (%d): %s" failure
                       (string-trim completion))
            (acm-backend-devonthink--remember
             query (acm-backend-devonthink--parse completion))
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (acm-update)))))
      (when (buffer-live-p (process-buffer process))
        (kill-buffer (process-buffer process))))))

(defun acm-backend-devonthink--search (query buffer)
  "Ask DEVONthink for QUERY in the background, refreshing BUFFER.
The previous process is killed, because its answer is for a search the user has
already typed past."
  (when (process-live-p acm-backend-devonthink--process)
    (kill-process acm-backend-devonthink--process))
  (setq acm-backend-devonthink--pending query)
  (setq acm-backend-devonthink--process
        (make-process
         :name "acm-devonthink"
         :noquery t
         :buffer (generate-new-buffer " *acm-devonthink*")
         ;; The error stream goes to the same buffer: osascript puts the reason
         ;; there, and the sentinel reports it when the exit status is not zero.
         :stderr (generate-new-buffer " *acm-devonthink-errors*")
         :command (list "osascript" "-e" (acm-backend-devonthink--script query))
         :sentinel (lambda (process event)
                     (acm-backend-devonthink--sentinel buffer query process event)))))

(defun acm-backend-devonthink-candidates (keyword)
  "Return the DEVONthink records whose name matches KEYWORD.
An AppleScript round trip takes too long for completion to wait for it, so the
search is debounced and its answer cached: the candidates of an earlier answer
are returned while a new one is on its way, and the answer itself refreshes the
menu through `acm-update'."
  (when (and acm-enable-devonthink
             (acm-backend-devonthink--trigger-p keyword))
    (let* ((query (acm-backend-devonthink--query keyword))
           (answer (cdr (assoc query acm-backend-devonthink--results)))
           (fresh (and answer
                       (< (- (float-time) (car answer))
                          acm-backend-devonthink-cache-seconds))))
      (when (and (not (string-empty-p query))
                 (not fresh)
                 (not (equal query acm-backend-devonthink--pending)))
        (when acm-backend-devonthink--timer
          (cancel-timer acm-backend-devonthink--timer))
        (let ((buffer (current-buffer)))
          (setq acm-backend-devonthink--timer
                (run-with-timer acm-backend-devonthink-debounce nil
                                (lambda ()
                                  (setq acm-backend-devonthink--timer nil)
                                  (acm-backend-devonthink--search query buffer))))))
      (cond
       ;; The trigger on its own has nothing to search for, and answering nil
       ;; would leave the user with no menu and no reason for it.
       ((and (string-empty-p query)
             (not (cdr answer)))
        (list (acm-backend-devonthink--hint)))
       (t
        (or (cdr answer)
            ;; The menu has to come up on the keystroke that starts the search,
            ;; so something is offered until the answer arrives.
            (when (equal query acm-backend-devonthink--pending)
              (list (acm-backend-devonthink--placeholder)))))))))

(defun acm-backend-devonthink-candidate-expand (candidate-info bound-start &optional preview)
  "Insert the org link of the DEVONthink record CANDIDATE-INFO.
The link points at the record through its UUID, which is the identifier the URL
scheme uses.  BOUND-START is where the prefix began; the trigger is removed as
well when it sits right before it, which is the case with the string prefix
style.  PREVIEW non-nil shows the link in an overlay instead of inserting it.
The placeholder that stands in while a search is in flight has no UUID, and
inserting it would put a link to nothing in the buffer, so it is ignored."
  (when (plist-get candidate-info :uuid)
    (let* ((text (format "[[%s://%s][%s]]"
                         acm-backend-devonthink-url-scheme
                         (plist-get candidate-info :uuid)
                         (plist-get candidate-info :label)))
           (trigger (length acm-backend-devonthink-trigger))
           (start (if (and (>= bound-start (1+ trigger))
                           (save-excursion
                             (goto-char bound-start)
                             (backward-char trigger)
                             (looking-at (regexp-quote acm-backend-devonthink-trigger))))
                      (- bound-start trigger)
                    bound-start)))
      (if preview
          (acm-preview-create-overlay start (point) text)
        (delete-region start (point))
        (insert text)))))

(defun acm-backend-devonthink--trigger-visible-p ()
  "Return non-nil when the text before point is the DEVONthink trigger.
The trigger ends in \":\", which is in `lsp-bridge-completion-hide-characters'
and would otherwise keep the menu from appearing right after it.  The popup is
asked for on every keystroke, so the trigger does not have to be the last thing
before point: \"dt:\", \"dt:f\" and \"dt:foo\" all count."
  (looking-back (concat (regexp-quote acm-backend-devonthink-trigger) "[^ \t\n]*")
                (line-beginning-position)))

(add-hook 'lsp-bridge-completion-hide-character-functions
          #'acm-backend-devonthink--trigger-visible-p)

(provide 'acm-backend-devonthink)

;;; acm-backend-devonthink.el ends here
