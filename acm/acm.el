;;; acm.el --- Asynchronous Completion Menu -*- lexical-binding: t -*-

;; Filename: acm.el
;; Description: Asynchronous Completion Menu
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2022, Andy Stewart, all rights reserved.
;; Created: 2022-05-31 16:29:33
;; Version: 0.1
;; Last-Updated: 2022-11-09 19:57:55 +0800
;;           By: Gong Qijian
;; URL: https://www.github.org/manateelazycat/acm
;; Keywords:
;; Compatibility: GNU Emacs 28.1
;;
;; Features that might be required by this library:
;;
;;
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
;; Asynchronous Completion Menu
;;

;;; Installation:
;;
;; Put acm.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'acm)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET acm RET
;;

;;; Change log:
;;
;; 2022/05/31
;;      * First released.
;;

;;; Acknowledgements:
;;
;;      * Daniel Mendler: reference some code from corfu, thanks
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'xml)
(require 'svg)
(require 'color)
(require 'subr-x)
(require 'cl-seq)
(require 'cl-lib)
(require 'cl-extra)
(require 'cl-macs)

(require 'acm-icon)
(require 'acm-frame)
(require 'acm-session)
;; `acm-popup' is a macro, so it has to be known while this file is compiled.
(eval-when-compile (require 'acm-session))
(require 'acm-backend)
(require 'acm-quick-access)

;;; Code:

(defgroup acm nil
  "Asynchronous Completion Menu."
  :prefix "acm-"
  :group 'lsp-bridge)

(defcustom acm-menu-length 10
  "Maximal number of candidates to show."
  :type 'integer
  :group 'acm)

(defcustom acm-menu-cycle t
  "If non-nil, selecting past the ends of the candidate list wraps around.
With this on, `acm-select-next' on the last candidate selects the first one
and `acm-select-prev' on the first selects the last, instead of stopping.
Only those two commands wrap; the page commands still stop at the ends."
  :type 'boolean
  :group 'acm)

(defcustom acm-hide-on-blank t
  "If non-nil, hide the menu when point ends up at a blank position.
A blank position is the start of a line or right after whitespace.  It is
checked after every command that runs while the menu is up,
because a deletion can uncover whitespace or the beginning of a line without
any command that `acm-continue-commands' would hide the menu for.  The
`lsp-bridge-completion-popup-predicates' decide whether completion may start
at such a position; this option only hides a menu that is already shown."
  :type 'boolean
  :group 'acm)

(defcustom acm-continue-commands
  ;; nil is undefined command
  '(nil ignore universal-argument universal-argument-more digit-argument
        self-insert-command org-self-insert-command
        ;; Avoid flashing completion menu when backward delete char
        grammatical-edit-backward-delete backward-delete-char-untabify
        python-indent-dedent-line-backspace delete-backward-char hungry-delete-backward
        "\\`acm-" "\\`scroll-other-window" "\\`special-lispy-" "\\`special-" "\\`lispy-")
  "Continue ACM completion after executing these commands."
  :type '(repeat (choice regexp symbol))
  :group 'acm)

(defcustom acm-enable-doc t
  "Whether to pop up the documentation of the selected candidate."
  :type 'boolean
  :group 'acm)

(defcustom acm-enable-doc-markdown-render 'async
  "When to render markdown styling in the documentation frame.
`async' renders it from an idle timer, t renders it immediately and
nil disables the rendering."
  :type '(choice (const :tag "Asynchronous" async)
                 (const :tag "Enabled" t)
                 (const :tag "Disabled" nil))
  :group 'acm)

(defcustom acm-enable-icon t
  "Show icon in completion menu."
  :type 'boolean
  :group 'acm)

(defcustom acm-enable-quick-access nil
  "Show quick-access in completion menu."
  :type 'boolean
  :group 'acm)

(defcustom acm-candidate-match-function 'regexp-quote
  "Function that turns the input prefix into a candidate matcher.
It is called with the downcased prefix and must return a regexp or
an `rx' form used to match candidate labels; nil matches every
candidate."
  :type '(choice
          (const nil)
          (const regexp-quote)
          (const orderless-flex)
          (const orderless-literal)
          (const orderless-prefixes)
          (const orderless-regexp)
          (const orderless-initialism))
  :group 'acm)

(defcustom acm-doc-frame-max-lines 20
  "Maximum number of lines of the documentation frame."
  :type 'integer
  :group 'acm)

(defcustom acm-doc-frame-boundary 'parent-frame
  "Boundary constraint for documentation frame.
`parent-frame' means constrain within parent Emacs frame.
`display' means constrain within display screen."
  :type '(choice (const :tag "Within parent Emacs frame" parent-frame)
                 (const :tag "Within display screen" display))
  :group 'acm)

(defcustom acm-frame-background-dark-color "#191a1b"
  "The frame background color for dark theme."
  :type 'string
  :group 'acm)

(defcustom acm-frame-background-light-color "#f0f0f0"
  "The frame background color for light theme."
  :type 'string
  :group 'acm)

(defvaralias 'acm-completion-backend-merge-order 'acm-backend-order)

(defcustom acm-backend-order
  '("mode-first-part-candidates"
    "template-first-part-candidates"
    "copilot-candidates"
    "template-second-part-candidates"
    "mode-second-part-candidates")
  "Order in which the candidate groups are concatenated in the menu.
Each element names one of the groups built by
`acm-update-candidates', such as \"mode-first-part-candidates\"."
  :type 'list
  :group 'acm)

(defcustom acm-completion-mode-candidates-merge-order
  '("elisp-candidates"
    "lsp-candidates"
    "lsp-workspace-symbol-candidates"
    "capf-candidates"
    "jupyter-candidates"
    "tabby-candidates"
    "ctags-candidates"
    "vulpea-candidates"
    "file-words-candidates"
    "telega-candidates")
  "Order in which the completion backends are asked for candidates.
Each element names one of the candidate groups consumed by
`acm-update-candidates', such as \"lsp-candidates\"."
  :type 'list
  :group 'acm)

(defcustom acm-enable-preview nil
  "Enable tab-and-go preview."
  :type 'boolean
  :group 'acm)

(cl-defmacro acm-run-idle-func (timer idle func)
  "Run FUNC after IDLE seconds of idle time, unless TIMER is live.
TIMER is a variable holding the idle timer; it is left untouched
when it is already non-nil, otherwise it is set to the new timer."
  `(unless ,timer
     (setq ,timer
           (run-with-idle-timer ,idle t #'(lambda () (funcall ,func))))))

(defvar acm-ui-overrides nil
  "Alist of function to the function that answers it instead here.
The menu of acm is drawn with child frames, which a terminal does not have; a
front end that draws it another way - `acm-terminal' is one - puts an entry for
each function it takes over here.  This is the extension point for that, rather
than advice on the functions of this package.")

(defun acm-ui-override (function)
  "Return the function taking FUNCTION over here, or nil.
The lookup is by `eq' on the symbol, and the value is what a front end left in
`acm-ui-overrides' for it."
  (cdr (assq function acm-ui-overrides)))

(cl-defmacro acm-with-cache-candidates (cache-candidates &rest body)
  "Return the list cached in CACHE-CANDIDATES, computing it if needed.
CACHE-CANDIDATES names a buffer-local variable holding a candidate
list.  When that variable is already non-nil it is returned as is,
otherwise BODY is evaluated, its value is stored in the variable and
returned."
  `(if (and (boundp ',cache-candidates)
            ,cache-candidates)
       ,cache-candidates
     (setq-local ,cache-candidates ,@body)
     ,cache-candidates))

(defvar acm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap next-line] #'acm-select-next)
    (define-key map [remap previous-line] #'acm-select-prev)
    (define-key map [down] #'acm-select-next)
    (define-key map [up] #'acm-select-prev)
    (define-key map "\M-n" #'acm-select-next)
    (define-key map "\M-p" #'acm-select-prev)
    (define-key map "\M-," #'acm-select-last)
    (define-key map "\M-." #'acm-select-first)
    (define-key map "\C-v" #'acm-select-next-page)
    (define-key map "\M-v" #'acm-select-prev-page)
    (define-key map [tab]  #'acm-complete)
    (define-key map "\C-m" #'acm-complete)
    (define-key map "\t" #'acm-complete)
    (define-key map "\n" #'acm-complete)
    (define-key map "\M-h" #'acm-complete)
    (define-key map "\M-H" #'acm-insert-common)
    (define-key map "\M-u" #'acm-filter)
    (define-key map "\M-d" #'acm-doc-toggle)
    (define-key map "\M-j" #'acm-doc-scroll-up)
    (define-key map "\M-k" #'acm-doc-scroll-down)
    (define-key map "\M-l" #'acm-hide)
    (define-key map "\C-g" #'acm-hide)
    map)
  "Keymap used when popup is shown.")

;; Only set `acm-mode-map' when acm-enable-quick-access is non-nil.
(when (and acm-enable-quick-access
           acm-quick-access-use-number-select)
  (dotimes (i 10)
    (define-key acm-mode-map (format "%d" i) #'acm-insert-number-or-complete-candiate)))

(defvar-local acm-candidates nil
  "All candidates collected by `acm-update-candidates' in this buffer.")
(defvar-local acm-menu-candidates nil
  "Current page of `acm-candidates' that is shown in the menu.")
(defvar-local acm-menu-index -1
  "Index of the selected candidate inside `acm-menu-candidates'.")
(defvar-local acm-menu-offset 0
  "Index of the first menu candidate inside `acm-candidates'.")

(defvar-local acm-input-bound-style "ascii"
  "How `acm-get-input-prefix-bound' delimits the completion prefix.
One of \"ascii\", \"symbol\", \"string\" or \"vulpea\"; `lsp-bridge'
also sets it per buffer from the prefix style of the backend.")

(defvar-local acm-filter-overlay nil
  "Overlay showing `acm-filter-string' at point, or nil when off.")
(defvar-local acm-filter-string ""
  "Characters typed while the acm filter is on.")

(defvar acm-preview-overlay nil
  "Overlay drawing the preview of the selected candidate, or nil.")

(defface acm-deprecated-face
  '((t :inherit shadow :strike-through t))
  "Face used for deprecated candidates.")

(defface acm-filter-face '()
  "Face used for the filter string shown by `acm-filter-update'.")

(defsubst acm-indent-pixel (xpos)
  "Return a display property that aligns to XPOS."
  `(space :align-to (,xpos)))

(define-minor-mode acm-mode
  "Minor mode whose map `acm-mode-map' is active while the menu is shown.
Toggling it also (re)installs the overriding keys of
`acm-overriding-key-setup'."
  :keymap acm-mode-map
  :init-value nil
  ;; Set override map, avoid some language mode map conflict with acm-mode map.
  (acm-overriding-key-setup))

(defvar acm-hide-hook nil
  "Hook run at the end of `acm-hide'.
The menu, the doc frame and the filter are gone by then, so a function here sees
the state the menu left behind - which is what lsp-bridge needs to reset what it
resolved for the items of that menu.")

;;;###autoload
(defun acm-hide ()
  "Hide the completion menu, the doc frame and the acm filter.
Turn off `acm-mode', remove the buffer-local `pre-command-hook' that
`acm-update' installed, forget the cached menu size of the session, ask the
backend of the current candidate to clean its cache and run `acm-hide-hook'.
A front end that draws the menu itself takes it over through `acm-ui-overrides'."
  (interactive)
  (catch 'acm-ui-override
    (let ((override (acm-ui-override 'acm-hide)))
      (when override
        (throw 'acm-ui-override (funcall override)))))
  (let* ((completion-menu-is-show-p (acm-frame-visible-p (acm-popup menu-frame)))
         (candidate-info (acm-menu-current-candidate))
         (backend (plist-get candidate-info :backend)))
    ;; Turn off `acm-mode'.
    (acm-mode -1)

    (acm-frame-hide-frame (acm-popup menu-frame))

    (acm-doc-hide)

    ;; Turn off acm filter.
    (acm-filter-off t)

    ;; Forget the cached menu size of the session.
    (acm-popup-set 'menu-max-length-cache 0)

    (when acm-preview-overlay
      (if (not (eq this-command 'acm-hide))
          ;; if `acm-hide' is called as command, not insert
          (acm-complete t)
        (delete-overlay acm-preview-overlay)
        (setq acm-preview-overlay nil)))

    ;; Remove hook of `acm--pre-command'.
    (remove-hook 'pre-command-hook #'acm--pre-command 'local)
    (remove-hook 'post-command-hook #'acm--post-command 'local)

    ;; If completion menu is show, clean backend cache.
    ;; Don't clean backend cache if menu is hide, to make sure completion menu have candidate when call command `lsp-bridge-popup-complete-menu'.
    (when completion-menu-is-show-p
      (acm-backend-clean backend))

    ;; Remove override map when hide acm menu.
    (acm-overriding-key-remove)

    ;; The menu is gone: tell whoever kept state for it.
    (run-hooks 'acm-hide-hook)))

(defun acm-overriding-key-setup ()
  "Some key define in language mode map will conflict with acm-mode map.
So we use `minor-mode-overriding-map-alist' to override key, make sure
all keys in acm-mode can response."
  (when (bound-and-true-p haskell-indentation-mode)
    (let ((override-map (make-sparse-keymap)))
      (define-key override-map [?\C-m] 'acm-complete)
      (set (make-local-variable 'minor-mode-overriding-map-alist)
           `((haskell-indentation-mode . ,override-map))))))

(defun acm-overriding-key-remove ()
  "We need remove override acm-mode keys, to make sure language mode key
can work after acm menu hide."
  (when (bound-and-true-p haskell-indentation-mode)
    (set (make-local-variable 'minor-mode-overriding-map-alist)
         (assq-delete-all 'haskell-indentation-mode minor-mode-overriding-map-alist))))

(defun acm-match-symbol-p (pattern sym)
  "Return non-nil if SYM is matching an element of the PATTERN list."
  (and (symbolp sym)
       (cl-loop for x in pattern
                thereis (if (symbolp x)
                            (eq sym x)
                          (string-match-p x (symbol-name sym))))))

(defun acm-in-vulpea-bracket-p ()
  "Return non-nil if point is inside a Vulpea note link bracket.
This is a WORKAROUND used to switch `acm-input-bound-style' to
\"vulpea\" locally, so that note titles - which are not symbols and may
contain non-ASCII characters - can be completed and expanded inside the
`[[' brackets of a link."
  (and acm-enable-vulpea
       (featurep 'vulpea)
       (eq major-mode 'org-mode)
       (not (org-in-src-block-p))
       ;; Only complete what is being typed: `org-in-regexp' also matches when
       ;; point merely sits on the opening brackets, e.g. *[[]].
       (when (org-in-regexp acm-backend-vulpea-bracket-re 1)
         (<= (match-beginning 2) (point)))))

(defun acm-get-input-prefix-bound ()
  "Return the buffer region that holds the completion prefix.
The delimiter depends on `acm-input-bound-style'.  Return nil when
there is no prefix at point."
  (cond
   ((equal "string" acm-input-bound-style)
    (cons (point)
          (save-excursion
            (if (search-backward-regexp "\\s-" (point-at-bol) t)
                (progn
                  (forward-char)
                  (point))
              (point-at-bol)))))
   ((equal "symbol" acm-input-bound-style)
    (bounds-of-thing-at-point 'symbol))
   ((equal "vulpea" acm-input-bound-style)
    (when (org-in-regexp acm-backend-vulpea-bracket-re 1)
      (cons (match-beginning 2)
            (match-end 2))))
   ((equal "ascii" acm-input-bound-style)
    (when-let* ((bound (bounds-of-thing-at-point 'symbol)))
      (let* ((keyword (buffer-substring-no-properties (car bound) (cdr bound)))
             (offset (or (string-match "[[:nonascii:]]+" (reverse keyword))
                         (length keyword))))
        (cons (- (cdr bound) offset) (cdr bound)))))))

(defun acm-get-input-prefix ()
  "Return the user input prefix at point.
The prefix is delimited by `acm-input-bound-style', except inside a Vulpea
note link, where the `vulpea' style is forced because a note title is not a
symbol.  Binding the style here is what keeps that workaround local, and it
must not shadow the value the caller set: that value is the option default or
the per buffer style `lsp-bridge' derives from the backend."
  (let* ((acm-input-bound-style (if (acm-in-vulpea-bracket-p)
                                    "vulpea"
                                  acm-input-bound-style))
         (bound (acm-get-input-prefix-bound)))
    (if bound
        (buffer-substring-no-properties (car bound) (cdr bound))
      "")))

(defun acm-candidate-fuzzy-search (keyword candidate)
  "Return non-nil if CANDIDATE matches KEYWORD.
Matching uses `acm-candidate-match-function' and is case
insensitive; when that function is nil every candidate matches."
  (if acm-candidate-match-function
      (let ((result (funcall acm-candidate-match-function (downcase keyword))))
        (string-match-p (cond ((stringp result) result)
                              (t (rx-to-string result))) ;; If `rx'.
                        (downcase candidate)))
    t))

(defun acm-candidate-sort-by-prefix (keyword candidates)
  "Sort CANDIDATES so that those starting with KEYWORD come first.
Candidates are then ordered by the length of their label.  Return
CANDIDATES unchanged when it is nil, is not a cons, or when KEYWORD
is nil."
  (when (and candidates
             (consp candidates))
    (if keyword
        (cl-sort
         candidates
         (lambda (a b)
           (let ((a-include-prefix (string-prefix-p keyword (plist-get a :label)))
                 (b-include-prefix (string-prefix-p keyword (plist-get b :label))))
             (cond
              ;; Sort by prefix first.
              ((and b-include-prefix (not a-include-prefix))
               nil)
              ((and a-include-prefix (not b-include-prefix))
               t)
              ;; Then sort by candidate length.
              (t
               (< (length (plist-get a :label))
                  (length (plist-get b :label))))))))
      ;; Don't sort candidates is keyword is empty string.
      candidates)))

(defvar-local acm-template-candidate-show-p nil
  "Non-nil when `acm-update-candidates' should fetch template candidates.")
(defvar-local acm-template-candidate-ticker 0
  "Counts how often `acm-template-candidate-init' ran in this buffer.")
(defvar-local acm-template-candidate-show-ticker 0
  "Value of `acm-template-candidate-ticker' when templates were shown.")
(defvar acm-template-candidate-timer nil
  "Timer delaying the next template candidate computation, or nil.")

(defun acm-template-candidate-init ()
  "Reset the template flag and count one more typing tick.
Call this from `lsp-bridge-try-completion' before a new completion;
templates are only recomputed after the user stopped typing, which
`acm-update-candidates' decides from the ticker."
  (setq-local acm-template-candidate-show-p nil)
  (setq-local acm-template-candidate-ticker (1+ acm-template-candidate-ticker)))

(defun acm-template-candidate-update ()
  "Set `acm-template-candidate-show-p' to t to calculate template candidates."
  (setq-local acm-template-candidate-show-p t)
  (acm-update))

(cl-defmacro acm-cancel-timer (timer)
  "Cancel TIMER and set the variable that holds it to nil.
Do nothing when TIMER is nil."
  `(when ,timer
     (cancel-timer ,timer)
     (setq ,timer nil)))

(defun acm-remove-duplicate-candidates (candidates lsp-labels)
  "Remove elements from CANDIDATES whose labels exist in LSP-LABELS."
  (cl-remove-if (lambda (candidate)
                  (member (plist-get candidate :label) lsp-labels))
                candidates))

(defun acm-update-template-candidates (keyword char-before-keyword lsp-candidates)
  "Return the yasnippet and tempel candidates for KEYWORD.
Return nil when templates must not be shown: inside a comment, or while
LSP-CANDIDATES is non-empty and CHAR-BEFORE-KEYWORD is not a completion
trigger character.  Whether the templates are fetched now, one tick later or
only after `acm-template-candidate-timer' fired is decided by the buffer-local
tickers, so that the menu does not flicker while the user is typing; the
candidates of both template backends are concatenated."
  (when (and (not (acm-in-comment-p))
             (or (zerop (length lsp-candidates))
                 (and (boundp 'acm-backend-lsp-completion-trigger-characters)
                      (not (member char-before-keyword
                                   acm-backend-lsp-completion-trigger-characters)))))
    (let (yas-candidates tempel-candidates)
      (cond
       ;; Templates are shown because the ticker asked for them.
       (acm-template-candidate-show-p
        (setq yas-candidates (acm-backend-candidates "yas" keyword)
              tempel-candidates (acm-backend-candidates "tempel" keyword))
        (setq-local acm-template-candidate-show-p nil)
        (setq-local acm-template-candidate-show-ticker acm-template-candidate-ticker))
       ;; Keep them while they were just shown, to avoid a flickering menu.
       ((= (- acm-template-candidate-ticker acm-template-candidate-show-ticker) 1)
        (setq yas-candidates (acm-backend-candidates "yas" keyword)
              tempel-candidates (acm-backend-candidates "tempel" keyword)))
       ;; Otherwise wait 200 ms, cancelling the previous wait.
       (t
        (acm-cancel-timer acm-template-candidate-timer)
        (setq acm-template-candidate-timer
              (run-with-timer 0.2 nil #'acm-template-candidate-update))))
      (append yas-candidates tempel-candidates))))

(defun acm-update-candidates ()
  "Collect, merge and filter the candidates for the current input.
Ask every enabled backend for candidates for the prefix at point, through the
registry of `acm-backend'; the mode candidates are appended in the order of
`acm-completion-mode-candidates-merge-order' and then split, so the first menu
page also holds the first template candidates.  The groups are concatenated in
the order of `acm-backend-order'.  When the path backend found something, its
candidates are the only ones, because a valid path at point is what the user
is typing.  With `acm-filter-overlay' set, only the candidates matching
`acm-filter-string' are kept.  Returns the candidate list, and advances the
buffer-local template ticker or arms `acm-template-candidate-timer' while
doing so."
  (let* ((keyword (acm-get-input-prefix))
         (char-before-keyword (save-excursion
                                (backward-char (length keyword))
                                (acm-char-before)))
         (queries (acm-backends-fetch keyword))
         (path-candidates (acm-backends-group queries "path-candidates"))
         candidates)
    (if (> (length path-candidates) 0)
        (setq candidates path-candidates)
      (acm-backends-dedupe-against-lsp queries)
      (let* ((lsp-candidates (acm-backends-group queries "lsp-candidates"))
             (mode-candidates (acm-backends-group-candidates
                               queries acm-completion-mode-candidates-merge-order))
             (template-candidates (acm-update-template-candidates
                                   keyword char-before-keyword lsp-candidates))
             (template-split-index (min (length template-candidates) 2))
             (template-first-part-candidates (cl-subseq template-candidates 0 template-split-index))
             (template-second-part-candidates (cl-subseq template-candidates template-split-index))
             ;; Keep the first menu page long enough for the templates that go
             ;; into it, so the mode candidates which follow are visible too.
             (mode-split-index (max (- acm-menu-length (length template-first-part-candidates)) 2))
             (mode-split (min (length mode-candidates) mode-split-index))
             (mode-first-part-candidates (cl-subseq mode-candidates 0 mode-split))
             (mode-second-part-candidates (cl-subseq mode-candidates mode-split)))
        (setq candidates
              (apply #'append
                     (mapcar (lambda (group-name)
                               (pcase group-name
                                 ("mode-first-part-candidates" mode-first-part-candidates)
                                 ("template-first-part-candidates" template-first-part-candidates)
                                 ("copilot-candidates" (acm-backends-group queries "copilot-candidates"))
                                 ("template-second-part-candidates" template-second-part-candidates)
                                 ("mode-second-part-candidates" mode-second-part-candidates)))
                             acm-backend-order)))))
    (if acm-filter-overlay
        (cl-remove-if-not (lambda (candidate)
                            (acm-candidate-fuzzy-search acm-filter-string
                                                        (plist-get candidate :label)))
                          candidates)
      candidates)))

(defun acm-menu-index-info (candidate)
  "Return a stable identity string for CANDIDATE.
The string joins the candidate label and backend with \"###\";
`acm-update' compares it to keep the selection on the same candidate
when a backend returns a new list."
  (format "%s###%s" (plist-get candidate :label) (plist-get candidate :backend)))

(defun acm-update (&optional candidate)
  "Render the completion menu for the current input.
Use CANDIDATE as the candidate list when it is non-nil, otherwise
call `acm-update-candidates'.  This stores `acm-candidates',
`acm-menu-candidates', `acm-menu-index' and `acm-menu-offset',
enables `acm-mode', records the popup position, and then either hides
the menu or renders it.  Return nil.  A front end that draws the menu itself
takes it over through `acm-ui-overrides'."
  (catch 'acm-ui-override
    (let ((override (acm-ui-override 'acm-update)))
      (when override
        (throw 'acm-ui-override (funcall override candidate)))))
  (acm-quick-access-init)

  ;; Adjust `gc-cons-threshold' to maximize temporary,
  ;; make sure Emacs not do GC
  (let* ((gc-cons-threshold most-positive-fixnum)
         (keyword (acm-get-input-prefix))
         (previous-select-candidate-index (+ acm-menu-offset acm-menu-index))
         (previous-select-candidate (acm-menu-index-info (acm-menu-current-candidate)))
         (candidates (or candidate (acm-update-candidates)))
         (menu-candidates (cl-subseq candidates 0 (min (length candidates) acm-menu-length)))
         (current-select-candidate-index (cl-position previous-select-candidate (mapcar 'acm-menu-index-info menu-candidates) :test 'equal))
         (acm-input-bound-style (if (acm-in-vulpea-bracket-p) "vulpea" "ascii"))
         (bounds (acm-get-input-prefix-bound)))
    (cond
     ;; Hide completion menu if there is only one exact match candidate, except when the candidate is a template.
     ((and (equal (length candidates) 1)
           (string-equal keyword (plist-get (nth 0 candidates) :label))
           (not (member (plist-get (nth 0 candidates) :annotation)
                        '("Emmet Abbreviation" "Snippet" "Yas-Snippet" "Tempel" "Vulpea"))))
      (acm-hide))
     ((> (length candidates) 0)
      (let* ((menu-old-cache (cons (acm-popup menu-max-length-cache) (acm-popup menu-number-cache))))
        ;; Enable acm-mode to inject mode keys.
        (acm-mode 1)

        ;; Use `pre-command-hook' to hide completion menu when command match `acm-continue-commands'.
        (add-hook 'pre-command-hook #'acm--pre-command nil 'local)
        ;; An option, because the pre command hook above cannot see a deletion that
        ;; uncovers whitespace: those commands are in `acm-continue-commands'.
        (when acm-hide-on-blank
          (add-hook 'post-command-hook #'acm--post-command nil 'local))

        ;; Adjust candidates.
        (setq-local acm-menu-offset 0)  ;init offset to 0
        (if (zerop (length menu-candidates))
            ;; Adjust `acm-menu-index' to -1 if no candidates found.
            (setq-local acm-menu-index -1)
          ;; First init `acm-menu-index' to 0.
          (setq-local acm-menu-index 0)

          ;; The following code is specifically to adjust the selection position of candidate when typing fast.
          (when (and current-select-candidate-index
                     (> (length candidates) 1))
            (cond
             ;; Swap the position of the first two candidates
             ;; if previous candidate's position change from 1st to 2nd.
             ((and (= previous-select-candidate-index 0) (= current-select-candidate-index 1))
              (cl-rotatef (nth 0 candidates) (nth 1 candidates))
              (cl-rotatef (nth 0 menu-candidates) (nth 1 menu-candidates)))
             ;; Swap the position of the first two candidates and select 2nd postion
             ;; if previous candidate's position change from 2nd to 1st.
             ((and (= previous-select-candidate-index 1) (= current-select-candidate-index 0))
              (cl-rotatef (nth 0 candidates) (nth 1 candidates))
              (cl-rotatef (nth 0 menu-candidates) (nth 1 menu-candidates))
              (setq-local acm-menu-index 1))
             ;; Select 2nd position if previous candidate's position still is 2nd.
             ((and (= previous-select-candidate-index 1) (= current-select-candidate-index 1))
              (setq-local acm-menu-index 1)))))

        (setq-local acm-candidates candidates)
        (setq-local acm-menu-candidates menu-candidates)

        (acm-frame-init-colors)

        ;; Record menu popup position and buffer.
        (acm-popup-set 'popup-point (or (car bounds) (point)))

        ;; `posn-at-point' will failed in CI, add checker make sure CI can pass.
        ;; CI don't need popup completion menu.
        (when (posn-at-point (acm-popup popup-point))
          (acm-popup-set 'popup-position
                         (acm-frame-get-popup-position (acm-popup popup-point)))

          ;; Create menu frame if it not exists, and render into it.  The writing
          ;; happens with `inhibit-read-only' bound: the mode owning the buffer being
          ;; completed in may have marked it read-only - telega marks the messages of
          ;; a chat that way - and the render reaches buffers of the session while
          ;; the current buffer is still the one being typed in, where a write that
          ;; only meant to reach the menu buffer raised `text-read-only' out of the
          ;; post-command hook.  What is written is the menu and the documentation,
          ;; in the buffers that hold them; the buffer being completed in is not
          ;; written to here.
          (let ((inhibit-read-only t))
            (acm-frame-create-frame-if-not-exist menu-frame (acm-popup menu-buffer) "acm frame" 0 t)

            (acm-menu-render menu-old-cache)))))
     (t
      (acm-hide)))))

(defun acm-reset-colors (&rest args)
  "Recompute the acm frame colors and redraw the frames in use.
ARGS are ignored; the function is added as `:after' advice to
`load-theme'."
  (acm-frame-init-colors t)

  ;; Reset the colors of every frame's popup, not only of this one's.
  (dolist (session (acm-session-all))
    (let ((menu-frame (acm-session-menu-frame session))
          (doc-frame (acm-session-doc-frame session)))
      (when (frame-live-p menu-frame)
        (acm-frame-set-frame-colors menu-frame)
        (when (frame-visible-p menu-frame)
          (with-current-buffer (acm-session-menu-buffer session)
            (acm-menu-render
             (cons (acm-session-menu-max-length-cache session)
                   (acm-session-menu-number-cache session))))))
      (when (frame-live-p doc-frame)
        (acm-frame-set-frame-colors doc-frame)))))

(if (daemonp)
    ;; The :background of 'default is unavailable until frame is created in
    ;; daemon mode.
    (add-hook 'server-after-make-frame-hook
              (lambda ()
                (advice-add #'load-theme :after #'acm-reset-colors)
                ;; Compensation for missing the first `load-thme' in
                ;; `after-init-hook'.
                (acm-reset-colors)))
  (advice-add #'load-theme :after #'acm-reset-colors))

;; Declared without a value: on a pgtk build this is Emacs's own variable, and this
;; function sets it to nought to keep child frames from waiting on events.
(defvar pgtk-wait-for-event-timeout)

(defun acm-running-in-wayland-native ()
  "Return non-nil if Emacs runs on a native Wayland display.
Checks for the pgtk window system and the GdkWaylandDisplay backend."
  (and (eq window-system 'pgtk)
       (fboundp 'pgtk-backend-display-class)
       (string-equal (pgtk-backend-display-class) "GdkWaylandDisplay")))

;; NOTE:
;; Emacs pgtk branch has bug https://debbugs.gnu.org/cgi/bugreport.cgi?bug=58556
;; we need set `pgtk-wait-for-event-timeout' to 0 to fix frame slow issue.
(when (acm-running-in-wayland-native)
  (setq pgtk-wait-for-event-timeout 0))

(defun acm-doc-hide ()
  "Hide the doc frame and drop any pending markdown render.
Thin wrapper around `acm-doc--hide'.  A front end that draws the documentation
itself takes it over through `acm-ui-overrides'."
  (let ((override (acm-ui-override 'acm-doc-hide)))
    (if override
        (funcall override)
      (acm-doc--hide))))

(defun acm-doc--hide()
  "Hide the documentation frame and clear the pending markdown render state.
Cancel the pending markdown render and forget the rendered text."
  (acm-frame-hide-frame (acm-popup doc-frame))

  ;; A timer kept in a session slot is cancelled through the slot: `acm-cancel-timer'
  ;; takes a variable to cancel and to set to nil, and a slot is not one - handing it one
  ;; made it expand to `(setq (acm-popup markdown-timer) nil)', which is the crash this
  ;; replaces.
  (when-let* ((timer (acm-popup markdown-timer)))
    (cancel-timer timer)
    (acm-popup-set 'markdown-timer nil))

  (acm-popup-set 'markdown-doc nil))

(defun acm--at-blank-position-p ()
  "Return non-nil when point is at the start of a line or after whitespace.
Whitespace is decided by the syntax table, where `char-syntax' yields ?\s only
for whitespace syntax, so tabs count and no other syntax class is mixed in."
  (or (bolp)
      (eq (char-syntax (or (char-before) 0)) ?\s)))

(defun acm--post-command ()
  "Hide the menu when `acm-hide-on-blank' is set and point is at blank.
Installed on the buffer-local `post-command-hook' while the menu is up, so a
deletion that uncovers whitespace or the start of a line hides it too."
  (when (and acm-hide-on-blank
             (acm-frame-visible-p (acm-popup menu-frame))
             (acm--at-blank-position-p))
    (acm-hide)))

(defun acm--pre-command ()
  "Hide the completion menu for commands outside `acm-continue-commands'.
Installed on the buffer-local `pre-command-hook' by `acm-update'."
  ;; Use `pre-command-hook' to hide completion menu when command match `acm-continue-commands'.
  (unless (acm-match-symbol-p acm-continue-commands this-command)
    (acm-hide)))

;;;###autoload
(defun acm-complete (&optional not-hide)
  "Insert the selected candidate into the buffer.
Call the expand function the backend of the candidate is registered
with when it has one, otherwise replace the input prefix by the candidate
label.  Delete `acm-preview-overlay' and, unless NOT-HIDE
is non-nil, hide the menu and the doc frame."
  (interactive)
  ;; The write goes with `inhibit-read-only' bound: the token being completed can
  ;; sit in text a mode has marked read-only - telega marks the messages of a chat
  ;; that way - and taking a candidate writes over that token and nothing else.
  (let ((inhibit-read-only t))
    (let* ((candidate-info (acm-menu-current-candidate))
           (bound-start (acm-popup popup-point)))
      (if (acm-backend-expand-p candidate-info)
          (acm-backend-expand candidate-info bound-start)
        (delete-region bound-start (point))
        (insert (plist-get candidate-info :label)))))

  (when (overlayp acm-preview-overlay)
    (delete-overlay acm-preview-overlay))
  (setq acm-preview-overlay nil)

  ;; Hide menu and doc frame after complete candidate.
  (unless not-hide
    (acm-hide)))

(defun acm-preview-create-overlay (beg end display)
  "Create a preview overlay covering BEG to END in the selected window.
DISPLAY is inserted as the `display' property, but only when it is a
string.  Return the new overlay."
  (let ((ov (make-overlay beg end nil)))
    (overlay-put ov 'priority 1000)
    (overlay-put ov 'window (selected-window))
    (when (stringp display)
      (overlay-put ov 'display display))
    ov))

(defun acm-preview-current ()
  "Preview the selected candidate in the current buffer.
Call the candidate-expand function of the backend with its PREVIEW
argument when the backend supports previewing, otherwise overlay the
candidate label from the popup point on.  The overlay is stored in
`acm-preview-overlay' and the menu frame is moved when the preview
made it taller."
  (let* ((candidate-info (acm-menu-current-candidate))
         (beg (acm-popup popup-point))
         (cand (plist-get candidate-info :label)))
    (when acm-preview-overlay (delete-overlay acm-preview-overlay))
    (if (acm-backend-preview-p candidate-info)
        (save-excursion
          (setq acm-preview-overlay (acm-backend-expand candidate-info beg t)))
      (setq acm-preview-overlay (acm-preview-create-overlay beg (point) cand)))
    ;; adjust pos of menu frame.
    (when-let* ((popup-pos (acm-frame-get-popup-position
                           (acm-popup popup-point)
                           (1- (length (split-string (overlay-get acm-preview-overlay 'display) "\n")))))
               ((not (eq (cdr popup-pos) (cdr (acm-popup popup-position))))))
      (setcdr (acm-popup popup-position) (cdr popup-pos))
      (acm-menu-adjust-pos))))

;;;###autoload
(defun acm-complete-or-expand-yas-snippet ()
  "Complete the candidate when the menu is visible, else expand a snippet.
Bind this to <tab> in `yas-keymap'; it calls `acm-complete' while
the menu frame is visible, and `yas-next-field-or-maybe-expand'
otherwise."
  (interactive)
  (if (acm-frame-visible-p (acm-popup menu-frame))
      (acm-complete)
    (yas-next-field-or-maybe-expand)))

;;;###autoload
(defun acm-insert-common ()
  "Insert common prefix of menu."
  (interactive)
  (when (acm-frame-visible-p (acm-popup menu-frame))
    (let* ((common-string "")
           (items (mapcar (lambda (v) (plist-get v :label)) acm-menu-candidates))
           (item-min-length (cl-reduce #'min (mapcar #'string-width items)))
           (input-prefix (acm-get-input-prefix)))
      (dolist (index (number-sequence 0 (1- item-min-length)))
        (let* ((char-list (mapcar (lambda (i) (substring i index (1+ index))) items))
               (first-char (cl-first char-list)))
          (when (cl-every (lambda (x) (string-equal x first-char)) char-list)
            (setq common-string (concat common-string first-char)))))

      (if (and (> (length common-string) 0)
               (> (length common-string) (length input-prefix)))
          ;; Written with the same binding as taking a candidate: what is written is
          ;; the rest of the word being completed.
          (let ((inhibit-read-only t))
            (insert (substring common-string (length (acm-get-input-prefix)))))
        (message "No common string found")))))

(defvar acm-string-width-function
  (if (fboundp 'string-pixel-width)
      'string-pixel-width
    'string-width)
  "Function used to measure the width of candidate strings.
Bound to `string-pixel-width' when it exists and to `string-width'
otherwise; read by `acm-menu-max-length' and
`acm-menu-render-items'.")

(defun acm-menu-max-length ()
  "Get max length of menu candidates, use for adjust menu size dynamically.
A front end that draws the menu itself takes it over through `acm-ui-overrides'
and is given the length computed here to adjust."
  (let ((return (cl-reduce #'max
                           (mapcar (lambda (v)
                                     (funcall acm-string-width-function
                                              (format "%s %s"
                                                      (plist-get v :displayLabel)
                                                      (plist-get v :annotation))))
                                   acm-menu-candidates))))
    (let ((override (acm-ui-override 'acm-menu-max-length)))
      (if override (funcall override return) return))))

(defun acm-menu-render-items (items menu-index)
  "Insert one line per candidate of ITEMS into the current buffer.
MENU-INDEX is the index of the selected line, which is highlighted
with `acm-frame-select-face'.  Icons, quick-access keys and the
right-aligned annotation column are rendered, and the last newline is
removed.  Return nil.  A front end that draws the menu itself takes it
over through `acm-ui-overrides'."
  (catch 'acm-ui-override
    (let ((override (acm-ui-override 'acm-menu-render-items)))
      (when override
        (throw 'acm-ui-override (funcall override items menu-index)))))
  (let* ((item-index 0)
         (annotation-not-exits (cl-every (lambda (item) (string-empty-p (plist-get item :annotation))) items)))
    (dolist (v items)
      (let* ((icon (cdr (assoc (downcase (plist-get v :icon)) acm-icon-alist)))
             (icon-default (cdr (assoc t acm-icon-alist)))
             (display-icon (or icon icon-default))
             (candidate (plist-get v :displayLabel))
             (annotation (plist-get v :annotation))
             (annotation-text (format "%s " (capitalize (if annotation annotation ""))))
             (annotation-face (if (equal item-index menu-index) 'acm-frame-select-face 'font-lock-doc-face))
             (item-length (funcall acm-string-width-function annotation-text))
             (icon-text (acm-icon-build (nth 0 display-icon) (nth 1 display-icon) (nth 2 display-icon)))
             (quick-access-key (nth item-index acm-quick-access-keys))
             candidate-line)

        (when (plist-get v :deprecated)
          (add-face-text-property 0 (length candidate) 'acm-deprecated-face 'append candidate))

        (setq candidate-line
              (concat
               ;; Icon.
               (if acm-enable-icon
                   icon-text   ; render icon if `acm-enable-icon' is t
                 " ")   ; add left padding if `acm-enable-icon' is nil
               ;; Index.
               (when acm-enable-quick-access
                 (if quick-access-key (concat quick-access-key ". ") "   "))
               ;; Candidate.
               candidate
               ;; Fill in the blank according to the maximum width, make sure marks align right of menu.
               (unless annotation-not-exits
                 (propertize " "
                             'display
                             (acm-indent-pixel
                              (if (equal acm-string-width-function 'string-pixel-width)
                                  (- (+ (acm-popup menu-max-length-cache) (* 20 (string-pixel-width " "))) item-length)
                                (ceiling (* (frame-char-width) (- (+ (acm-popup menu-max-length-cache) 20) item-length)))))))
               (propertize annotation-text 'face annotation-face)
               "\n"))

        (when (equal item-index menu-index)
          (add-face-text-property 0 (length candidate-line) 'acm-frame-select-face 'append candidate-line)

          ;; Hide doc frame if some backend not support fetch candidate documentation.
          (when (and
                 (not (acm-backend-doc-p v))
                 (acm-frame-visible-p (acm-popup doc-frame)))
            (acm-doc-hide)))

        ;; Insert candidate line.
        (insert candidate-line)

        ;; Delete the last extra return line.
        (when (equal item-index (1- (length items)))
          (delete-backward-char 1))

        (setq item-index (1+ item-index))))))

(defun acm-menu-adjust-pos ()
  "Adjust menu frame position."
  (let* ((emacs-width (frame-pixel-width))
         (emacs-height (frame-pixel-height))
         (acm-frame-width (frame-pixel-width (acm-popup menu-frame)))
         (acm-frame-height (frame-pixel-height (acm-popup menu-frame)))
         (cursor-x (car (acm-popup popup-position)))
         (cursor-y (cdr (acm-popup popup-position)))
         (offset-x (* (frame-char-width) acm-icon-width))
         (offset-y (line-pixel-height))
         (acm-frame-x (if (> (+ cursor-x acm-frame-width) emacs-width)
                          (max (- cursor-x acm-frame-width) offset-x)
                        (max (- cursor-x offset-x) 0)))
         (acm-frame-y (cond
                       ;; Always popup up when cursor in minibuffer.
                       ((minibufferp)
                        (- (nth 1 (window-pixel-edges (minibuffer-window))) acm-frame-height))
                       ;; Popup up when acm menu bottom below at Emacs bottom edge.
                       ((> (+ cursor-y acm-frame-height) emacs-height)
                        (- cursor-y acm-frame-height))
                       ;; Otherwise, popup down.
                       (t
                        (+ cursor-y offset-y)))))
    (acm-frame-set-frame-position (acm-popup menu-frame) acm-frame-x acm-frame-y)))

(defun acm-doc-try-show (&optional update-completion-item)
  "Show the documentation of the current candidate in the doc frame.
Ask the backend for the documentation of the current candidate and,
when it is non-empty and `acm-enable-doc' is non-nil, insert it in
the documentation buffer, maybe render it as markdown according to
`acm-enable-doc-markdown-render' and adjust the doc frame.  A
non-nil UPDATE-COMPLETION-ITEM hides the doc frame when the backend
is \"lsp\"; other backends always hide it here.  The backend is asked
for the documentation even when `acm-enable-doc' is nil, because
`completion_item_resolve' also fetches the `additionalTextEdits' used
by auto import.  A front end that draws the documentation itself takes it over
through `acm-ui-overrides'."
  (catch 'acm-ui-override
    (let ((override (acm-ui-override 'acm-doc-try-show)))
      (when override
        (throw 'acm-ui-override (funcall override update-completion-item)))))
  ;; We need call `acm-backend-*-candidate-doc' function even option `acm-enable-doc' is nil,
  ;; because `completion_item_resolve' will fetch `additionalTextEdits', otherwise, auto import feature is miss.
  (let* ((candidate (acm-menu-current-candidate))
         (backend (plist-get candidate :backend))
         (candidate-doc (acm-backend-doc candidate)))
    (when acm-enable-doc
      (if (or (consp candidate-doc) ; If the type fo snippet is set to command, then the "doc" will be a list.
              (and (stringp candidate-doc) (not (string-empty-p candidate-doc))))
          (let ((doc (if (stringp candidate-doc)
                         candidate-doc
                       (format "%S" candidate-doc))))
            ;; Create doc frame if it not exist.
            (acm-frame-create-frame-if-not-exist doc-frame (acm-popup doc-buffer) "acm doc frame" 1 t)
            (acm-popup-set 'doc-hide-p nil)

            ;; Insert documentation and turn on wrap line.
            (with-current-buffer (get-buffer-create (acm-popup doc-buffer))
              (read-only-mode -1)
              (when (not (string-equal doc (acm-popup markdown-doc)))
                (erase-buffer)
                (insert doc))
              (visual-line-mode 1))

            ;; Only render markdown styling when idle 200ms, because markdown render is expensive.
            (when (member backend '("lsp" "copilot"))
              (when-let* ((timer (acm-popup markdown-timer)))
                (cancel-timer timer)
                (acm-popup-set 'markdown-timer nil))
              (cl-case acm-enable-doc-markdown-render
                (async (acm-popup-set 'markdown-timer
                             (run-with-idle-timer 0.2 nil
                                                  (lambda ()
                                                    (acm-doc-markdown-render-content doc)))))
                ((t) (acm-doc-markdown-render-content doc))))

            ;; Adjust doc frame position and size.
            (acm-doc-frame-adjust))

        (pcase backend
          ;; If backend is LSP, doc frame hide when `update-completion-item' is t.
          ("lsp" (when update-completion-item
                   (acm-doc-hide)))
          ;; Hide doc frame immediately if backend is not LSP.
          (_ (acm-doc-hide)))))))

(defun acm-doc-frame-adjust-display-boundary ()
  "Adjust doc frame position when using display boundary."
  (let* ((acm-frame-width (frame-pixel-width (acm-popup menu-frame)))
         (acm-frame-height (frame-pixel-height (acm-popup menu-frame)))
         ;; Get menu frame position relative to its parent
         (acm-frame-pos (frame-position (acm-popup menu-frame)))
         (acm-frame-x (car acm-frame-pos))
         (acm-frame-y (cdr acm-frame-pos))
         ;; Get parent frame absolute position
         (parent-frame (frame-parameter (acm-popup menu-frame) 'parent-frame))
         (parent-abs-pos (and parent-frame (cdr (assq 'outer-position (frame-geometry parent-frame)))))
         (parent-abs-x (if parent-abs-pos (car parent-abs-pos) 0))
         (parent-abs-y (if parent-abs-pos (cdr parent-abs-pos) 0))
         ;; Calculate menu frame absolute position
         (menu-abs-x (+ parent-abs-x acm-frame-x))
         (menu-abs-y (+ parent-abs-y acm-frame-y))
         ;; Calculate distances to display edges
         (acm-frame-left-distance menu-abs-x)
         (acm-frame-right-distance (- (display-pixel-width) menu-abs-x acm-frame-width))
         (acm-frame-top-distance menu-abs-y)
         (acm-frame-bottom-distance (- (display-pixel-height) menu-abs-y acm-frame-height))
         (acm-doc-frame-max-width
          (min (max acm-frame-left-distance acm-frame-right-distance)
               (frame-pixel-width)))
         (acm-doc-frame-max-height
          (min (max acm-frame-top-distance acm-frame-bottom-distance)
               (frame-pixel-height))))

    ;; Convert pixel-size into char-size
    (acm-frame-set-frame-max-size (acm-popup doc-frame)
                                  (ceiling (/ acm-doc-frame-max-width (frame-char-width)))
                                  (min (ceiling (/ acm-doc-frame-max-height (window-default-line-height)))
                                       acm-doc-frame-max-lines))
    ;; Adjust doc frame with it's size.
    (let* ((acm-doc-frame-width (frame-pixel-width (acm-popup doc-frame)))
           (acm-doc-frame-height (frame-pixel-height (acm-popup doc-frame)))
           ;; Calculate doc frame position (relative coordinates)
           (acm-doc-frame-x (if (> (+ menu-abs-x acm-frame-width acm-doc-frame-max-width) (display-pixel-width))
                                (- acm-frame-x acm-doc-frame-width)
                              (+ acm-frame-x acm-frame-width)))
           (acm-doc-frame-y (if (> (+ menu-abs-y acm-doc-frame-height) (display-pixel-height))
                                (if (> acm-frame-top-distance acm-frame-bottom-distance)
                                    (max 0 (- acm-frame-y acm-doc-frame-height))
                                  ;; Place at display bottom, need to convert to relative coordinates
                                  (- (display-pixel-height) acm-doc-frame-height parent-abs-y))
                              acm-frame-y)))

      (acm-frame-set-frame-position (acm-popup doc-frame) acm-doc-frame-x acm-doc-frame-y))))

(defun acm-doc-frame-adjust-parent-boundary ()
  "Adjust doc frame position when using parent frame boundary."
  (let* ((acm-frame-width (frame-pixel-width (acm-popup menu-frame)))
         (acm-frame-height (frame-pixel-height (acm-popup menu-frame)))
         (acm-frame-pos (frame-position (acm-popup menu-frame)))
         (acm-frame-x (car acm-frame-pos))
         (acm-frame-y (cdr acm-frame-pos))
         (acm-frame-left-distance acm-frame-x)
         (acm-frame-right-distance (- (frame-pixel-width) acm-frame-x acm-frame-width))
         (acm-frame-top-distance acm-frame-y)
         (acm-frame-bottom-distance (- (frame-pixel-height) acm-frame-y acm-frame-height))
         (acm-doc-frame-max-width
          (min (max acm-frame-left-distance acm-frame-right-distance)
               (frame-pixel-width)))
         (acm-doc-frame-max-height
          (min (max acm-frame-top-distance acm-frame-bottom-distance)
               (frame-pixel-height))))

    ;; Convert pixel-size into char-size
    (acm-frame-set-frame-max-size (acm-popup doc-frame)
                                  (ceiling (/ acm-doc-frame-max-width (frame-char-width)))
                                  (min (ceiling (/ acm-doc-frame-max-height (window-default-line-height)))
                                       acm-doc-frame-max-lines))
    ;; Adjust doc frame with it's size.
    (let* ((acm-doc-frame-width (frame-pixel-width (acm-popup doc-frame)))
           (acm-doc-frame-height (frame-pixel-height (acm-popup doc-frame)))
           (acm-doc-frame-x (if (> (+ acm-frame-x acm-frame-width acm-doc-frame-max-width) (frame-pixel-width))
                                (- acm-frame-x acm-doc-frame-width)
                              (+ acm-frame-x acm-frame-width)))
           (acm-doc-frame-y (if (> (+ acm-frame-y acm-doc-frame-height) (frame-pixel-height))
                                (if (> acm-frame-top-distance acm-frame-bottom-distance)
                                    (max 0 (- acm-frame-y acm-doc-frame-height))
                                  (- (frame-pixel-height) acm-doc-frame-height))
                              acm-frame-y)))
      (acm-frame-set-frame-position (acm-popup doc-frame) acm-doc-frame-x acm-doc-frame-y))))

(defun acm-doc-frame-adjust ()
  "Adjust doc frame position based on boundary configuration."
  (if (eq acm-doc-frame-boundary 'parent-frame)
      (acm-doc-frame-adjust-parent-boundary)
    (acm-doc-frame-adjust-display-boundary)))

(defun acm-menu-current-candidate ()
  "Get current candidate with menu index and offset."
  (nth (+ acm-menu-offset acm-menu-index) acm-candidates))

(defun acm-menu-render (menu-old-cache)
  "Render the menu in the session menu buffer and resize and place its frame.
MENU-OLD-CACHE is a cons of the max candidate length and the
candidate number of the previous render, as held by
the cached menu size and candidate number; the frame is
only resized when they differ from the new values.  The cache
variables are updated here, and `acm-doc-try-show' is called at the
end.  A front end that draws the menu itself takes it over through
`acm-ui-overrides'."
  (catch 'acm-ui-override
    (let ((override (acm-ui-override 'acm-menu-render)))
      (when override
        (throw 'acm-ui-override (funcall override menu-old-cache)))))
  (let* ((items acm-menu-candidates)
         (menu-old-max-length (car menu-old-cache))
         (menu-old-number (cdr menu-old-cache))
         (menu-new-max-length (acm-menu-max-length))
         (menu-new-number (length items))
         (menu-index acm-menu-index))
    (acm-popup-set 'menu-max-length-cache menu-new-max-length)
    (acm-popup-set 'menu-number-cache menu-new-number)

    ;; Insert menu candidates.
    (with-current-buffer (get-buffer-create (acm-popup menu-buffer))
      (erase-buffer)
      (acm-menu-render-items items menu-index))

    ;; Not adjust menu frame size if not necessary,
    ;; such as select candidate just change index,
    ;; or menu width not change when switch to next page.
    (when (or (not (equal menu-old-max-length menu-new-max-length))
              (not (equal menu-old-number menu-new-number)))
      (acm-frame-set-frame-max-size (acm-popup menu-frame))

      ;; Adjust doc frame with menu frame position.
      (when (acm-frame-visible-p (acm-popup doc-frame))
        (acm-doc-frame-adjust)))

    ;; Adjust menu frame position.
    ;; We need adjust acm menu position after set it size, avoid coordinate shifts due to resizing.
    (acm-menu-adjust-pos)

    ;; Fetch `documentation' and `additionalTextEdits' information.
    (acm-doc-try-show)))

(cl-defmacro acm-silent (&rest body)
  "Silence BODY."
  (declare (indent 0))
  `(cl-letf ((inhibit-message t)
             (message-log-max nil)
             ((symbol-function #'minibuffer-message) #'ignore))
     (ignore-errors ,@body)))

(defun acm-menu-update-candidates ()
  "Replace `acm-menu-candidates' by the page at `acm-menu-offset'.
Do nothing when `acm-candidates' is not longer than the current
`acm-menu-candidates'."
  (let ((menu-length (length acm-menu-candidates)))
    ;; Only change menu candidates when filter candidate length bigger than menu length.
    (when (> (length acm-candidates) menu-length)
      (setq-local acm-menu-candidates
                  (cl-subseq acm-candidates acm-menu-offset (+ acm-menu-offset menu-length))))))

(cl-defmacro acm-menu-update (&rest body)
  "Evaluate BODY and redraw the menu when the selection moved.
BODY is expected to change `acm-menu-index' or `acm-menu-offset';
both are clamped to valid values afterwards, the menu is re-rendered
only when one of them changed, and `acm-preview-current' is called
when `acm-enable-preview' is non-nil."
  `(let ((inhibit-read-only t))
     (let* ((menu-old-index acm-menu-index)
          (menu-old-offset acm-menu-offset)
            (menu-old-cache (cons (acm-popup menu-max-length-cache) (acm-popup menu-number-cache))))
       ,@body

       (cond ((< acm-menu-index 0)
              (setq-local acm-menu-index 0))
             ((>= acm-menu-index (length acm-menu-candidates))
              (setq-local acm-menu-index (1- (length acm-menu-candidates)))))

       (cond ((< acm-menu-offset 0)
              (setq-local acm-menu-offset 0))
             ((>= acm-menu-offset (- (length acm-candidates) (length acm-menu-candidates)))
              (setq-local acm-menu-offset (- (length acm-candidates) (length acm-menu-candidates)))))

       ;; Only update menu candidates when menu index or offset changed.
       (when (or (not (equal menu-old-index acm-menu-index))
                 (not (equal menu-old-offset acm-menu-offset)))
         (acm-menu-update-candidates)
         (acm-menu-render menu-old-cache))
       (when acm-enable-preview
         (acm-preview-current)))))

(defun acm-char-before ()
  "Return the character before point as a string.
Return an empty string at the beginning of the buffer."
  (let ((prev-char (char-before)))
    (if prev-char (char-to-string prev-char) "")))

(defvar-local acm-is-elisp-mode-in-org nil
  "Non-nil when the current Org buffer should complete as Emacs Lisp.
Set by `lsp-bridge-org-babel' inside Org source blocks and read by
`acm-is-elisp-mode-p'.")
(defun acm-is-elisp-mode-p ()
  "Return non-nil if the current buffer completes Emacs Lisp.
True in `emacs-lisp-mode', `inferior-emacs-lisp-mode' and
`lisp-interaction-mode', in an Org buffer where
`acm-is-elisp-mode-in-org' is set, and in a minibuffer whose local
map binds `completion-at-point'."
  (or (derived-mode-p 'emacs-lisp-mode)
      (derived-mode-p 'inferior-emacs-lisp-mode)
      (derived-mode-p 'lisp-interaction-mode)
      (and (eq major-mode 'org-mode)
           acm-is-elisp-mode-in-org)
      (and (minibufferp)
           (where-is-internal #'completion-at-point (list (current-local-map))))))

(defun acm--cycle-next-p ()
  "Return non-nil when `acm-select-next' has to wrap to the first candidate.
That is the case when `acm-menu-cycle' is set and the selected candidate is
the last one of the whole list, not only of the visible page."
  (and acm-menu-cycle
       (= acm-menu-index (1- (length acm-menu-candidates)))
       (= (+ acm-menu-offset acm-menu-index) (1- (length acm-candidates)))))

(defun acm--cycle-prev-p ()
  "Return non-nil when `acm-select-prev' has to wrap to the last candidate.
That is the case when `acm-menu-cycle' is set and the first candidate of the
whole list is selected."
  (and acm-menu-cycle
       (= acm-menu-index 0)
       (= acm-menu-offset 0)))

;;;###autoload
(defun acm-select-first ()
  "Select first candidate."
  (interactive)
  (acm-menu-update
   (setq-local acm-menu-offset 0)
   (setq-local acm-menu-index 0)))

;;;###autoload
(defun acm-select-last ()
  "Select last candidate."
  (interactive)
  (acm-menu-update
   (let ((menu-length (length acm-menu-candidates)))
     (setq-local acm-menu-offset (- (length acm-candidates) menu-length))
     (setq-local acm-menu-index (- menu-length 1)))))

;;;###autoload
(defun acm-select-next ()
  "Select next candidate.
With `acm-menu-cycle' non-nil, the last candidate wraps to the first."
  (interactive)
  (if (acm--cycle-next-p)
      (acm-select-first)
    (acm-menu-update
     (cond ((< acm-menu-index (1- (length acm-menu-candidates)))
            (setq-local acm-menu-index (1+ acm-menu-index)))
           ((< (+ acm-menu-offset acm-menu-index) (1- (length acm-candidates)))
            (setq-local acm-menu-offset (1+ acm-menu-offset)))))))

;;;###autoload
(defun acm-select-prev ()
  "Select previous candidate.
With `acm-menu-cycle' non-nil, the first candidate wraps to the last."
  (interactive)
  (if (acm--cycle-prev-p)
      (acm-select-last)
    (acm-menu-update
     (cond ((> acm-menu-index 0)
            (setq-local acm-menu-index (1- acm-menu-index)))
           ((> acm-menu-offset 0)
            (setq-local acm-menu-offset (1- acm-menu-offset)))))))

;;;###autoload
(defun acm-select-next-page ()
  "Select next page candidate."
  (interactive)
  (acm-menu-update
   (cond ((< acm-menu-index (1- (length acm-menu-candidates)))
          (setq-local acm-menu-index (+ acm-menu-index acm-menu-length)))
         ((< (+ acm-menu-offset acm-menu-index) (1- (length acm-candidates)))
          (setq-local acm-menu-offset (+ acm-menu-offset acm-menu-length))))))

;;;###autoload
(defun acm-select-prev-page ()
  "Select previous page candidate."
  (interactive)
  (acm-menu-update
   (cond ((> acm-menu-index 0)
          (setq-local acm-menu-index (- acm-menu-index acm-menu-length)))
         ((> acm-menu-offset 0)
          (setq-local acm-menu-offset (- acm-menu-offset acm-menu-length))))))

;;;###autoload
(defun acm-doc-scroll-up ()
  "Scroll the doc frame buffer up by one screen.
A front end that draws the documentation itself takes it over through
`acm-ui-overrides'."
  (interactive)
  (let ((override (acm-ui-override 'acm-doc-scroll-up)))
    (if override
        (funcall override)
      (with-current-buffer (acm-popup doc-buffer)
        (when (framep (acm-popup menu-frame))
          (with-selected-frame (acm-popup doc-frame)
            (scroll-up-command)))))))

;;;###autoload
(defun acm-doc-scroll-down ()
  "Scroll the doc frame buffer down by one screen.
A front end that draws the documentation itself takes it over through
`acm-ui-overrides'."
  (interactive)
  (let ((override (acm-ui-override 'acm-doc-scroll-down)))
    (if override
        (funcall override)
      (with-current-buffer (acm-popup doc-buffer)
        (when (framep (acm-popup menu-frame))
          (with-selected-frame (acm-popup doc-frame)
            (scroll-down-command)))))))

;;;###autoload
(defun acm-doc-toggle ()
  "Toggle documentation preview for selected candidate."
  (interactive)
  (if (acm-frame-visible-p (acm-popup doc-frame))
      (acm-doc-hide)
    (let ((acm-enable-doc t))
      (acm-doc-try-show))))

(defvar acm-markdown-render-prettify-symbols-alist
  (nconc
   (cl-loop for i from 0 to 255
            collect (cons (format "&#x%02X;" i) i))
   '(("\\!" . ?!) ("\\#" . ?#) ("\\*" . ?*) ("\\+" . ?+) ("\\:" . ?:)
     ("\\<" . ?<) ("\\>" . ?>) ("\\[" . ?\[) ("\\]" . ?\]) ("\\^" . ?^)
     ("\\_" . ?_) ("\\`" . ?`) ("\\|" . ?|) ("\\~" . ?~) ("\\\\" . ?\\)
     ("&lt;" . ?<) ("&gt;" . ?>) ("&amp;" . ?&)))
  "Prettify symbol alist used to render markdown in the doc buffer.
Maps HTML entities and backslash escapes to their characters; it is
assigned to `prettify-symbols-alist' by
`acm-markdown-render-content'.")

(defun acm-frame-background-color ()
  "Return the acm frame background color for the current theme.
Use `acm-frame-background-dark-color' when the frame background mode
is \"dark\", and `acm-frame-background-light-color' when it is
\"light\"."
  (pcase (format "%s" (frame-parameter nil 'background-mode))
    ("dark" acm-frame-background-dark-color)
    ("light" acm-frame-background-light-color)))

(defun acm-markdown-render-content (&optional enable-decorations)
  "Render the current buffer as markdown documentation.
Turn on `gfm-view-mode' when it is available, remap
`markdown-code-face' to the frame background, prettify symbols and
fontify the buffer.  Unless ENABLE-DECORATIONS is non-nil, also turn
off line numbers and the mode line.  Return nil."
  (when (fboundp 'gfm-view-mode)
    (let ((inhibit-message t))
      ;; Enable `gfm-view-mode' first, otherwise `gfm-view-mode' will change attribute of face `markdown-code-face'.
      (gfm-view-mode)

      ;; Then remapping background and height of `markdown-code-face' to same as acm doc buffer.
      (face-remap-add-relative
       'markdown-code-face
       :background (face-attribute 'default :background (acm-popup menu-frame)))))

  (read-only-mode 0)

  (setq prettify-symbols-alist acm-markdown-render-prettify-symbols-alist)
  (setq prettify-symbols-compose-predicate (lambda (_start _end _match) t))
  (prettify-symbols-mode 1)

  ;; Syntax Highlight.
  (font-lock-ensure)

  (unless enable-decorations
    (display-line-numbers-mode -1)
    (setq-local mode-line-format nil)))

(defun acm-doc-markdown-render-content (doc)
  "Render the markdown styling of DOC in the session documentation buffer.
Do nothing when the doc frame is not visible or when DOC is already
the rendered markdown text.  Otherwise render it and,
when `acm-enable-doc-markdown-render' is `async', adjust the doc
frame again because `gfm-view-mode' changes the content."
  (when (and (acm-frame-visible-p (acm-popup doc-frame))
             (not (string-equal doc (acm-popup markdown-doc))))
    (with-current-buffer (get-buffer-create (acm-popup doc-buffer))
      (read-only-mode -1)
      (acm-markdown-render-content))

    (acm-popup-set 'markdown-doc doc)

    ;; We need `acm-doc-frame-adjust' again if `acm-enable-doc-markdown-render' is `async'.
    ;; otherwise, `gfm-view-mode' will remove ``` line.
    (cl-case acm-enable-doc-markdown-render
      (async (acm-doc-frame-adjust)))))

(defun acm-in-comment-p (&optional state)
  "Return non-nil if point is inside a comment.
Use tree-sitter when a parser is available, otherwise use the syntax
STATE when it is non-nil and `acm-current-parse-state' otherwise.
Return nil at the beginning or the end of the buffer."
  (if (and (featurep 'treesit) (treesit-available-p) (treesit-parser-list))
      ;; Avoid use `acm-current-parse-state' when treesit is enable.
      ;; `beginning-of-defun' is very expensive function will slow down completion menu.
      ;; We use `treesit-node-type' directly if treesit is enable.
      (or (eq (get-text-property (point) 'face) 'font-lock-comment-face)
          (string-equal (treesit-node-type (treesit-node-at (point))) "comment"))
    (ignore-errors
      (unless (or (bobp) (eobp))
        (save-excursion
          (or
           (nth 4 (or state (acm-current-parse-state)))
           (eq (get-text-property (point) 'face) 'font-lock-comment-face)))))))

(defun acm-in-string-p (&optional state)
  "Return non-nil if point is inside a string.
Use tree-sitter when a parser is available, otherwise use the syntax
STATE when it is non-nil and `acm-current-parse-state' otherwise.
Return nil at the beginning or the end of the buffer."
  (if (and (featurep 'treesit) (treesit-available-p) (treesit-parser-list))
      ;; Avoid use `acm-current-parse-state' when treesit is enable.
      ;; `beginning-of-defun' is very expensive function will slow down completion menu.
      ;; We use `treesit-node-type' directly if treesit is enable.
      (or (eq (get-text-property (point) 'face) 'font-lock-string-face)
          (string-equal (treesit-node-type (treesit-node-at (point))) "string"))
    (ignore-errors
      (unless (or (bobp) (eobp))
        (save-excursion
          (and
           (nth 3 (or state (acm-current-parse-state)))
           (not (equal (point) (line-end-position)))))))))

(defun acm-current-parse-state ()
  "Return the `parse-partial-sexp' state of the buffer at point.
Parse from the beginning of the defun, or from the beginning of the
line when point is already at the beginning of the defun."
  (let ((point (point)))
    (beginning-of-defun)
    (when (equal point (point))
      (beginning-of-line))
    (parse-partial-sexp (point) point)))

;;;###autoload
(defun acm-filter ()
  "Toggle the acm filter in the current buffer.
Turn it off when `acm-filter-overlay' is set, and on otherwise."
  (interactive)
  (if acm-filter-overlay
      (acm-filter-off)
    (acm-filter-on)))

(defun acm-filter-on ()
  "Turn on the acm filter and echo a message about it.
Create `acm-filter-overlay' at point, where `acm-filter-insert-char'
collects the typed characters into `acm-filter-string'."
  (setq-local acm-filter-overlay (make-overlay (point) (point)))
  (message "Turn on acm filter."))

(defun acm-filter-off (&optional quiet)
  "Turn off the acm filter.
Delete `acm-filter-overlay' and reset `acm-filter-string'; unless
QUIET is non-nil, also echo a message."
  (when acm-filter-overlay
    (delete-overlay acm-filter-overlay))
  (setq-local acm-filter-overlay nil)
  (setq-local acm-filter-string "")
  (unless quiet
    (message "Turn off acm filter.")))

;;;###autoload
(defun acm-filter-insert-char ()
  "Insert filter character."
  (interactive)
  (when acm-filter-overlay
    ;; Append current character in `acm-filter-string'.
    (setq-local acm-filter-string (concat acm-filter-string (format "%s" (this-command-keys))))

    (acm-filter-update)))

;;;###autoload
(defun acm-filter-delete-char ()
  "Delete filter character."
  (interactive)
  (when acm-filter-overlay
    (if (length= acm-filter-string 0)
        (acm-filter-off)
      ;; Delete last char of `acm-filter-string'
      (setq-local acm-filter-string (substring acm-filter-string 0 -1))

      (acm-filter-update))))

(defun acm-filter-update ()
  "Redraw the filter overlay string and refresh the candidate list.
Recolor `acm-filter-face', put `acm-filter-string' after
`acm-filter-overlay' and call `acm-update' so that the candidates are
filtered again."
  (when (facep 'hl-line)
    (set-face-background 'acm-filter-face (face-attribute 'hl-line :background)))
  (set-face-foreground 'acm-filter-face (face-attribute 'font-lock-function-name-face :foreground))

  ;; Change overlay string.
  (overlay-put acm-filter-overlay
               'after-string
               (propertize acm-filter-string
                           'face
                           'acm-filter-face))

  ;; Filter candiates.
  (acm-update))

;; Emacs 28: Do not show Acm commands with M-X
(dolist (sym '(acm-hide acm-complete acm-select-first acm-select-last acm-select-next
                        acm-select-prev acm-insert-common acm-doc-scroll-up acm-doc-scroll-down
                        acm-complete-quick-access acm-doc-toggle))
  (put sym 'completion-predicate #'ignore))

(provide 'acm)
;;; acm.el ends here
