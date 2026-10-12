;;; acm-terminal.el --- Patch for LSP bridge acm on Terminal -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2022/07/07
;; Version: 0.1.0
;; Last-Updated: 2023-12-06 19:41:42 +0800
;;           By: Gong Qijian
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/twlz0ne/acm-terminal
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Draw the acm completion menu and its documentation in a terminal, where the
;; child frames acm draws them in do not exist; `popon' puts overlays at the end
;; of a line instead.  Loading this file changes nothing: call
;; `acm-terminal-active' to take the menu over and `acm-terminal-deactive' to give
;; it back, although the two are already called for you when Emacs runs in a
;; terminal (see the end of the file).
;;
;; The file was written by Gong Qijian as the `acm-terminal' package
;; (https://github.com/twlz0ne/acm-terminal, commit 1851d8f) and merged into
;; lsp-bridge so that the two are maintained and released together.

;; ## Requirements

;; - [popon](https://codeberg.org/akib/emacs-popon), to draw the popup without
;;   child frames.  Loading this file works without it; taking the menu over does
;;   not, and says so.

;; ## Installation

;; Clone or download this repository (path of the folder is the `<path-to-acm-terminal>` used below).

;; ## Configuration

;; ```emacs-lisp
;; (require 'yasnippet)
;; (yas-global-mode 1)

;; (require 'lsp-bridge)
;; (global-lsp-bridge-mode)

;; (unless (display-graphic-p)
;;   (add-to-list 'load-path "<path-to-acm-terminal>")
;;   (with-eval-after-load 'acm
;;     (require 'acm-terminal)))
;; ```

;;; Code:

(require 'subr-x)
(require 'acm)

;; `popon' is what draws a popup at the end of a line in a terminal.  It is not a
;; dependency of lsp-bridge, so a machine without it can still load every file of
;; the package; its absence is reported by `acm-terminal-active', the only thing
;; that needs it.
(defvar acm-terminal--popon-available (require 'popon nil t)
  "The `popon' feature when it is installed, nil otherwise.")

;; `popon' is what draws the popup at the end of a line; it is not loaded here
;; when it is missing (see the commentary), so the compiler is told about it.
(declare-function popon-create "popon" (content position &optional window buffer priority))
(declare-function popon-kill "popon" (popon))
(declare-function popon-live-p "popon" (popon))
(declare-function popon-position "popon" (popon))
(declare-function popon-redisplay "popon" (&optional popon))
(declare-function popon-size "popon" (popon))
(declare-function popon-x-y-at-pos "popon" (position))

(defvar acm-terminal-min-width 45
  "The minimum width of the candidate menu in characters.")

(defvar acm-terminal-max-width 100
  "The maximum width of the candidate menu in characters.")

(defvar acm-terminal-doc-continuation-string "\\"
  "The string showing at the end of wrapped lines.")

(defvar acm-terminal-doc-min-width 40
  "The minimum width of the candidate doc in characters.")

(defvar acm-terminal-doc-max-width 80
  "The maximum width of the candidate doc in characters.")

(defvar acm-terminal-menu-popon nil
  "The `popon' the candidate menu is drawn in, or nil when it is not shown.
A terminal has no child frames, so the menu here is an overlay at the end of a
line instead; this is where that overlay is kept between updates.")

(defvar acm-terminal-doc-popon nil
  "The `popon' the documentation of a candidate is drawn in, or nil.
See `acm-terminal-menu-popon' for why the documentation is an overlay here.")

(defvar-local acm-terminal-candidate-doc nil
  "Latest docstring.")

(defvar-local acm-terminal-doc-scroll-start 0
  "Start of doc scrolling.")

(defvar-local acm-terminal-current-input nil
  "Curent input.")

(defgroup acm-terminal nil
  "Draw the acm menu in a terminal, where child frames do not exist."
  :prefix "acm-terminal-"
  :group 'acm)

(defface acm-terminal-default-face
  '()
  "Default face for Terminal."
  :group 'acm-terminal)

(defface acm-terminal-select-face
  '()
  "Default select face for Terminal."
  :group 'acm-terminal)

(defun acm-terminal-line-number-display-width ()
  "Return width of line number bar."
  (if (bound-and-true-p display-line-numbers-mode)
      (+ (line-number-display-width) 2)
    0))

(defun acm-terminal-nsplit-string (string width &optional cont)
  "Split STRING into substrings of length WIDTH, padded with spaces.

Every substring is WIDTH characters wide, the last one included; it is padded
with spaces when the text does not fill it.  If CONT is non-nil it is appended
to every substring except the last, and the text of each is then WIDTH minus
the width of CONT, e.g.:

  (fn \"foobarbazq\" 3 \"↩\") => (\"fo↩\" \"ob↩\" \"ar↩\" \"ba↩\" \"zq \")
  (fn \"foobarbazq\" 3)     => (\"foo\" \"bar\" \"baz\" \"q  \") "
  (let* ((cont (or cont ""))
         (cont-width (string-width cont))
         last-column)
    (with-temp-buffer
      (insert string)
      (when (acm-popup markdown-timer)
        (cl-letf (((symbol-function 'window-body-width) (lambda (&rest _) width)))
          (acm-markdown-render-content)))
      (goto-char (point-min))
      (while (not (eobp))
        (setq last-column (current-column))
        (forward-char)
        (when (eq ?\t (char-before (point)))
          (let ((tab-width (- (current-column) last-column)))
            (delete-char -1)
            (insert (make-string tab-width ?\s))))
        (when (and (not (eolp)) (<= width (current-column)))
          (unless ;; Backward if there is not a pagebreak line
              (and (acm-popup markdown-timer)
                   (let ((props (text-properties-at (1- (point)))))
                     (and (memq 'display props) (memq 'markdown-hr-face props))))
            (backward-char cont-width))
          (insert cont "\n")))
      (mapcar (lambda (line)
                (truncate-string-to-width line width 0 ?\s))
              (split-string (buffer-string) "\n")))))

(defun acm-terminal-default-background ()
  (or (face-background 'acm-terminal-default-face)
      (pcase (face-background 'default)
        ("unspecified-bg" "white")
        (`,color color))))

(defun acm-terminal-init-colors (&optional force)
  (let* ((is-dark-mode (string-equal (acm-frame-get-theme-mode) "dark"))
         (blend-background (if is-dark-mode "#000000" "#AAAAAA"))
         (default-background (acm-terminal-default-background)))
    ;; Make sure menu follow the theme of Emacs.
    (when (or force (equal (face-attribute 'acm-terminal-default-face :background) 'unspecified))
      (set-face-background 'acm-terminal-default-face (acm-frame-color-blend default-background blend-background (if is-dark-mode 0.8 0.9))))
    (when (or force (equal (face-attribute 'acm-terminal-select-face :background) 'unspecified))
      (set-face-background 'acm-terminal-select-face (acm-frame-color-blend default-background blend-background 0.6)))
    (when (or force (equal (face-attribute 'acm-terminal-select-face :foreground) 'unspecified))
      (set-face-foreground 'acm-terminal-select-face (face-attribute 'font-lock-function-name-face :foreground)))))

(defun acm-terminal-get-popup-position ()
  "Return postion of menu."
  (if (and acm-terminal-menu-popon (eobp))
      ;; The existing overlay will cause `popon-x-y-at-pos' and `posn-x-y' to
      ;; get the wrong position when point at the and of buffer.
      (let ((pos (popon-position acm-terminal-menu-popon))
            (direction (plist-get (cdr acm-terminal-menu-popon) :direction))
            (size (popon-size acm-terminal-menu-popon)))
        (cons (car pos)
              (if (eq 'top direction)
                  (+ (cdr pos) (cdr size))
                (1- (cdr pos)))))
    (let ((pos (popon-x-y-at-pos (acm-popup popup-point))))
      (if (eobp)
          (cons (car pos) (1+ (cdr pos)))
        pos))))

(defun acm-terminal-popon-visible-p (popon)
  "Return non-nil when POPON is live and its property list says it is shown."
  (when (popon-live-p popon)
    (plist-get (cdr popon) :visible)))

(defun acm-terminal-make-popon (text pos &optional window buffer priority)
  "Create an invisible popon with TEXT at POS of WINDOW.
See `popon-create' for more information."
  (cl-letf (((symbol-function 'popon-update) #'ignore))
    (popon-create text pos window buffer priority)))

(defun acm-terminal-make-frame (_)
  "Advice override `acm-make-frame' to make an invisible popon."
  (let ((pos (acm-terminal-get-popup-position)))
    (acm-terminal-make-popon (cons "" 0) pos)))

(cl-defmacro acm-terminal-create-frame-if-not-exist (frame _frame-buffer _frame-name &optional _internal-border)
  `(unless (popon-live-p ,frame)
     (setq ,frame (acm-terminal-make-frame nil))))

(defun acm-terminal-max-length (return)
  "Return the menu width a terminal should use for the candidate width RETURN.
The menu is two columns wider than the candidates need, and no wider than the
window; `acm-menu-max-length' calls this with the width it computed."
  (min (+ return 2)
       (- (window-width)
          (+ (- (car (window-inside-edges)) (window-left-column))
             (acm-terminal-line-number-display-width)))))

(defun acm-terminal-menu-render-items (items menu-index)
  (let* ((item-index 0)
         ;; The max length is calcuated base on the format
         ;; bellow (see `acm-menu-max-length'):
         ;;
         ;;   {label}\s{annotation}
         ;;
         ;; but in Terminal we render the items in a different way, so the
         ;; calculation format should be:
         ;;
         ;;   {label}\s{annotation}\s
         ;;
         ;; without changing the format, then we should add 1 when using
         ;; `(acm-popup menu-max-length-cache)'.
         (max-length (1- (acm-popup menu-max-length-cache)))
         (_annotation-not-exits (cl-every (lambda (item) (string-empty-p (plist-get item :annotation))) items)))
    (dolist (v items)
      (let* ((icon (cdr (assoc (downcase (plist-get v :icon)) acm-icon-alist)))
             (candidate (plist-get v :displayLabel))
             (candidate-length (funcall acm-string-width-function candidate))
             (annotation (plist-get v :annotation))
             (annotation-text (if annotation annotation ""))
             (annotation-length (funcall acm-string-width-function annotation-text))
             (candidate-max-length (- max-length annotation-length 2))
             (padding-length (- max-length (+ candidate-length annotation-length) 2))
             (_icon-text (if icon (acm-icon-build (nth 0 icon) (nth 1 icon) (nth 2 icon)) ""))
             (quick-access-key (nth item-index acm-quick-access-keys))
             candidate-line)

        ;; Render deprecated candidate.
        (when (plist-get v :deprecated)
          (add-face-text-property 0 (length candidate) 'acm-deprecated-face 'append candidate))

        ;; Build candidate line.
        (setq candidate-line
              (concat
               ;; icon-text
               (when acm-enable-quick-access
                 (if quick-access-key (concat quick-access-key ". ") "   "))
               (if (zerop padding-length)
                   candidate
                 (if (> padding-length 0)
                     (concat candidate (make-string padding-length ?\s))
                   (truncate-string-to-width candidate candidate-max-length
                                             0 ?\s)))
               " "
               (propertize (format "%s \n" (capitalize annotation-text))
                           'face
                           (if (equal item-index menu-index) 'acm-terminal-select-face 'font-lock-doc-face))))

        ;; Render current candidate.
        (if (equal item-index menu-index)
            (progn
              (add-face-text-property 0 (length candidate-line) 'acm-terminal-select-face 'append candidate-line)

              ;; Hide doc frame if some backend not support fetch candidate documentation.
              (when (and
                     (not (member (plist-get v :backend) '("lsp" "elisp" "yas")))
                     (acm-frame-visible-p acm-terminal-doc-popon))
                (acm-doc-hide)))
          (add-face-text-property 0 (length candidate-line) 'acm-terminal-default-face 'append candidate-line))

        ;; Insert candidate line.
        (insert candidate-line)

        ;; Delete the last extra return line.
        (when (equal item-index (1- (length items)))
          (delete-char -1))

        ;; Update item index.
        (setq item-index (1+ item-index))))))

(defun acm-terminal-doc-render (doc &optional width)
  "Render DOC string."
  (when (and (stringp doc) (not (string-empty-p doc)))
    (let ((width (or width (1- acm-terminal-doc-max-width))))
      (mapcar
       (lambda (line)
         (add-face-text-property 0 (length line) 'acm-terminal-default-face 'append line)
         line)
       (acm-terminal-nsplit-string doc width acm-terminal-doc-continuation-string)))))

(defun acm-terminal-menu-render (menu-old-cache)
  (let* ((items acm-menu-candidates)
         (menu-old-max-length (car menu-old-cache))
         (menu-old-number (cdr menu-old-cache))
         (menu-new-max-length (acm-menu-max-length))
         (menu-new-number (length items))
         (menu-index acm-menu-index))
    ;; Record newest cache.
    (acm-popup-set 'menu-max-length-cache menu-new-max-length)
    (acm-popup-set 'menu-number-cache menu-new-number)

    ;; Insert menu candidates.
    (when acm-terminal-menu-popon
      (let ((lines (split-string
                    (with-temp-buffer
                      (acm-menu-render-items items menu-index)
                      (buffer-string))
                    "\n")))
        ;; Adjust menu frame position.
        (acm-terminal-menu-adjust-pos lines))

      (popon-redisplay)
      (plist-put (cdr acm-terminal-menu-popon) :visible t))

    ;; Not adjust menu frame size if not necessary,
    ;; such as select candidate just change index,
    ;; or menu width not change when switch to next page.
    (when (or (not (equal menu-old-max-length menu-new-max-length))
              (not (equal menu-old-number menu-new-number)))
      ;; Adjust doc frame with menu frame position.
      (when (acm-terminal-popon-visible-p acm-terminal-doc-popon)
        (acm-terminal-doc-adjust-pos acm-terminal-candidate-doc)))

    ;; Fetch `documentation' and `additionalTextEdits' information.
    (cl-letf (((symbol-function 'acm-frame-visible-p) 'acm-terminal-popon-visible-p))
      (acm-terminal-doc-try-show))))

(defun acm-terminal-menu-adjust-pos (&optional lines)
  "Adjust menu frame position."
  (pcase-let* ((`(,edge-left ,edge-top ,_edge-right ,edge-bottom) (window-inside-edges))
               (textarea-width (- (window-width)
                                  (+ (- edge-left (window-left-column))
                                     (acm-terminal-line-number-display-width))))
               (_textarea-height (- edge-bottom edge-top))
               (`(,cursor-x . ,cursor-y)
                (prog1 (acm-terminal-get-popup-position)
                  (when lines
                    (plist-put (cdr acm-terminal-menu-popon) :lines lines)
                    (plist-put (cdr acm-terminal-menu-popon) :width (length (car lines))))))
               (`(,menu-w . ,menu-h) (popon-size acm-terminal-menu-popon))
               (bottom-free-h (- edge-bottom edge-top cursor-y)))
    (let ((x (if (> textarea-width (+ cursor-x menu-w))
                 cursor-x
               (- cursor-x (- (+ cursor-x menu-w) textarea-width) 1))))
      (plist-put (cdr acm-terminal-menu-popon) :x x))
    (cond
     ;; top
     ((<= bottom-free-h menu-h)
      (plist-put (cdr acm-terminal-menu-popon) :direction 'top)
      (plist-put (cdr acm-terminal-menu-popon) :y (- cursor-y menu-h)))
     ;; bottom
     (t
      (plist-put (cdr acm-terminal-menu-popon) :direction 'bottom)
      (plist-put (cdr acm-terminal-menu-popon) :y (+ cursor-y 1))))))

(defun acm-terminal-doc-get-page (lines start height)
  "Get doc page.

LINES   Doc lines
HEIGHT  Doc frame height
START   Start line"
  (let* ((taken-lines (seq-take (nthcdr start lines) height))
         (height-diff (- height (length taken-lines))))
    (if (< 0 height-diff)
        (append taken-lines
                (let ((blank-line (make-string (length (car taken-lines)) ?\s)))
                  (add-face-text-property
                   0 (length blank-line) 'acm-terminal-default-face 'append blank-line)
                  (make-list height-diff blank-line)))
      taken-lines)))

(defun acm-terminal-doc-top-edge-y (cursor-y menu-h doc-h &optional doc-lines)
  "Return the y-coordinate of doc at left/right top edge, set :lines if possible.

CURSOR-Y        y-coordinate of cursor
MENU-H          height of menu frame
DOC-H           height of doc frame
DOC-LINES       text lines of doc"
  (if (> doc-h cursor-y)
      ;; +---------y-------+--+
      ;; |         |       |  |
      ;; |         |       |  |
      ;; |  +------+       |  |
      ;; |  |      |       |  |
      ;; |  +-menu-+~~doc~~+  |
      ;; |  a|     :.......:  |
      ;; +--------------------+
      (prog1 0
        (when doc-lines
          (plist-put (cdr acm-terminal-doc-popon) :lines (seq-take doc-lines cursor-y))))
    (if (> doc-h menu-h)
        ;; +--------------------+
        ;; |         y-------+  |
        ;; |         |       |  |
        ;; |  +------+       |  |
        ;; |  |      |       |  |
        ;; |  +-menu-+--doc--+  |
        ;; |  a|                |
        ;; +--------------------+
        (- cursor-y doc-h)
      ;; +--------------------+
      ;; |  +------y-------+  |
      ;; |  |      |       |  |
      ;; |  |      +--doc--+  |
      ;; |  |      |          |
      ;; |  +-menu-+          |
      ;; |  a|                |
      ;; +--------------------+
      (- cursor-y menu-h))))

(defun acm-terminal-doc-adjust-pos (&optional candidate-doc)
  "Adjust doc frame position."
  (pcase-let* ((`(,edge-left ,edge-top ,_edge-right ,edge-bottom) (window-inside-edges))
               (textarea-width (- (window-width)
                                  (+ (- edge-left (window-left-column))
                                     (acm-terminal-line-number-display-width))))
               (textarea-height (- edge-bottom edge-top))
               (`(,_cursor-x . ,cursor-y) (acm-terminal-get-popup-position))
               (`(,menu-x . ,menu-y) (popon-position acm-terminal-menu-popon))
               (`(,menu-w . ,menu-h) (popon-size acm-terminal-menu-popon))
               (menu-right (+ menu-x menu-w))
               (doc-w nil)
               (doc-h nil)
               (doc-y nil)
               (doc-lines nil))
    (cond
     ;; l:menu + r:document
     ((>= textarea-width (+ menu-right acm-terminal-doc-max-width))
      (setq doc-lines (acm-terminal-doc-render candidate-doc))
      (if (eq 'bottom (plist-get (cdr acm-terminal-menu-popon) :direction))
          ;; right bottom
          (progn
            (setq doc-h (- textarea-height cursor-y))
            (setq doc-y (1+ cursor-y)))
        ;; right top
        (setq doc-h cursor-y)
        (setq doc-y (acm-terminal-doc-top-edge-y cursor-y menu-h (length doc-lines) doc-lines)))
      (plist-put (cdr acm-terminal-doc-popon) :width acm-terminal-doc-max-width)
      (plist-put (cdr acm-terminal-doc-popon) :x menu-right)
      (plist-put (cdr acm-terminal-doc-popon) :y doc-y)
      (plist-put (cdr acm-terminal-doc-popon) :lines
                                     (if (<= (length doc-lines) doc-h)
                                         ;; doc <= frame
                                         doc-lines
                                       ;; doc > frame
                                       (seq-take doc-lines doc-h))))
     (t
      (let* ((fix-width (min acm-terminal-doc-max-width (- textarea-width 1)))
             (rects
              (list
               (list 'right-bottom (- textarea-width menu-x menu-w) (- textarea-height cursor-y))
               (list 'right-top (- textarea-width menu-x menu-w) cursor-y)
               (list 'bottom fix-width (- edge-bottom edge-top menu-y menu-h))
               (list 'left-bottom menu-x (- textarea-height cursor-y))
               (list 'left-top menu-x cursor-y)
               (list 'top fix-width menu-y))))
        ;; Find the largest free space in left/top/bottom/right
        (pcase-let* ((`(,rect ,rect-width ,rect-height)
                      (car (seq-sort (lambda (r1 r2)
                                       (> (apply #'* (cdr r1)) (apply #'* (cdr r2))))
                                     (if acm-terminal-doc-min-width
                                         (seq-filter
                                          (lambda (r)
                                            (>= (cadr r) acm-terminal-doc-min-width))
                                          rects)
                                       rects))))
                     (rerender-width (- (min fix-width rect-width) 1)))
          (setq doc-lines (acm-terminal-doc-render candidate-doc rerender-width))
          (setq doc-h (length doc-lines)) ;; Update doc height
          (setq doc-w (1+ rerender-width))
          (plist-put (cdr acm-terminal-doc-popon) :lines doc-lines)
          (plist-put (cdr acm-terminal-doc-popon) :width doc-w)
          (pcase rect
            ('left-bottom
             (plist-put (cdr acm-terminal-doc-popon) :x (- menu-x doc-w))
             (plist-put (cdr acm-terminal-doc-popon) :y (1+ cursor-y)))
            ('left-top
             (plist-put (cdr acm-terminal-doc-popon) :x (- menu-x doc-w))
             (plist-put (cdr acm-terminal-doc-popon) :y (acm-terminal-doc-top-edge-y
                                                cursor-y menu-h doc-h doc-lines)))
            ('top
             (plist-put (cdr acm-terminal-doc-popon) :x (if (>= (- textarea-width menu-x) doc-w)
                                                   menu-x
                                                 (- textarea-width doc-w)))
             (plist-put (cdr acm-terminal-doc-popon)
                        :y (let* ((offset 0)
                                 (y (if (< menu-y cursor-y)
                                        ;; menu on top
                                        (- menu-y doc-h)
                                      ;; menu on bottom
                                      (- menu-y doc-h
                                         (if (eq 'bottom (plist-get (cdr acm-terminal-menu-popon) :direction))
                                             (setq offset 1)
                                           0)))))
                             (if (< y 0)
                                 (prog1 0
                                   (plist-put (cdr acm-terminal-doc-popon)
                                              :lines (seq-take doc-lines (+ doc-h y offset))))
                               y))))
            ('bottom
             (plist-put (cdr acm-terminal-doc-popon) :x (if (>= (- textarea-width menu-x) doc-w)
                                                   menu-x
                                                 (- textarea-width doc-w)))
             (plist-put (cdr acm-terminal-doc-popon) :y (+ menu-y menu-h
                                                  (if (eq 'top (plist-get (cdr acm-terminal-menu-popon) :direction))
                                                      1
                                                    0)))
             (plist-put (cdr acm-terminal-doc-popon) :lines (seq-take doc-lines rect-height)))
            ('right-bottom
             (plist-put (cdr acm-terminal-doc-popon) :x (+ menu-x menu-w))
             (plist-put (cdr acm-terminal-doc-popon) :y (1+ cursor-y)))
            ('right-top
             (plist-put (cdr acm-terminal-doc-popon) :x (+ menu-x menu-w))
             (plist-put (cdr acm-terminal-doc-popon) :y (acm-terminal-doc-top-edge-y
                                                cursor-y menu-h doc-h doc-lines))))))))
    (popon-redisplay)))

(defun acm-terminal-doc-hide ()
  "Hide the documentation overlay and drop a pending markdown render.
The render timer and the rendered documentation of the session are cleared the
same way acm clears them for its doc frame."
  (when (popon-live-p acm-terminal-doc-popon)
    (setq acm-terminal-doc-popon (popon-kill acm-terminal-doc-popon)))

  (let ((timer (acm-popup markdown-timer)))
    (when timer
      (cancel-timer timer)))
  (acm-popup-set 'markdown-timer nil)
  (acm-popup-set 'markdown-doc nil))

(defun acm-terminal-doc-try-show (&optional update-completion-item)
  (when acm-enable-doc
    (let* ((candidate (acm-menu-current-candidate))
           (backend (plist-get candidate :backend))
           (candidate-doc-func (intern-soft (format "acm-backend-%s-candidate-doc" backend)))
           (candidate-doc
            (when (fboundp candidate-doc-func)
              (funcall candidate-doc-func candidate))))
      (setq acm-terminal-candidate-doc candidate-doc)
      (setq acm-terminal-doc-scroll-start 0)
      (if (or (consp candidate-doc) ; If the type fo snippet is set to command,
                                        ; then the "doc" will be a list.
              (and (stringp candidate-doc) (not (string-empty-p candidate-doc))))
          (let ((doc (if (stringp candidate-doc)
                         candidate-doc
                       (format "%S" candidate-doc))))
            ;; Create doc frame if it not exist.
            (acm-terminal-create-frame-if-not-exist acm-terminal-doc-popon acm-doc-buffer "acm doc frame")

            ;; Adjust doc frame position and size.
            (if (string-equal backend "lsp")
                (progn
                  ;; NOTE: It is imposible to do it as in the GUI:
                  ;; Insert doc first, then render in timer.
                  (let ((timer (acm-popup markdown-timer)))
                        (when timer
                          (cancel-timer timer)))
                  (acm-popup-set 'markdown-timer
                        (run-with-idle-timer
                         0.2 nil #'acm-terminal-doc-adjust-pos doc)))
              (acm-terminal-doc-adjust-pos doc)))

        (pcase backend
          ;; If backend is LSP, doc frame hide when `update-completion-item' is t.
          ("lsp" (when update-completion-item
                   (acm-doc-hide)))
          ;; Hide doc frame immediately if backend is not LSP.
          (_ (acm-doc-hide)))))))

(defun acm-terminal-doc-scroll-up ()
  "Scroll text of doc upward."
  (interactive)
  (when (popon-live-p acm-terminal-doc-popon)
    (pcase-let*
        ((`(,_doc-x . ,_doc-y) (popon-position acm-terminal-doc-popon))
         (`(,doc-w . ,doc-h) (popon-size acm-terminal-doc-popon))
         (total-lines
          (acm-terminal-doc-render acm-terminal-candidate-doc (1- doc-w)))
         (scroll-start
          (- (+ acm-terminal-doc-scroll-start doc-h) next-screen-context-lines))
         (taken-lines
          (when (< scroll-start (length total-lines))
            (acm-terminal-doc-get-page total-lines scroll-start doc-h))))
      (when taken-lines
        (plist-put (cdr acm-terminal-doc-popon) :lines taken-lines)
        (popon-redisplay)
        (setq acm-terminal-doc-scroll-start scroll-start)))))

(defun acm-terminal-doc-scroll-down ()
  "Scroll text of doc down."
  (interactive)
  (when (popon-live-p acm-terminal-doc-popon)
    (pcase-let*
        ((`(,_doc-x . ,_doc-y) (popon-position acm-terminal-doc-popon))
         (`(,doc-w . ,doc-h) (popon-size acm-terminal-doc-popon))
         (total-lines
          (acm-terminal-doc-render acm-terminal-candidate-doc (1- doc-w)))
         (scroll-start
          (+ (- acm-terminal-doc-scroll-start doc-h) next-screen-context-lines))
         (taken-lines
          (when (<= 0 scroll-start)
            (acm-terminal-doc-get-page total-lines scroll-start doc-h))))
      (when taken-lines
        (plist-put (cdr acm-terminal-doc-popon) :lines taken-lines)
        (popon-redisplay)
        (setq acm-terminal-doc-scroll-start scroll-start)))))

(defun acm-terminal-hide ()
  (interactive)
  (let* ((candidate-info (acm-menu-current-candidate))
         (backend (plist-get candidate-info :backend)))
    ;; Turn off `acm-mode'.
    (acm-mode -1)

    ;; Hide menu frame.
    (when acm-terminal-menu-popon
      (setq acm-terminal-menu-popon (popon-kill acm-terminal-menu-popon)))

    ;; Hide doc frame.
    (acm-doc-hide)

    ;; Clean `(acm-popup menu-max-length-cache)'.
    (acm-popup-set 'menu-max-length-cache 0)

    ;; Remove hook of `acm--pre-command'.
    (remove-hook 'pre-command-hook #'acm--pre-command 'local)

    ;; Clean backend cache.
    (when-let* ((backend-clean (intern-soft (format "acm-backend-%s-clean" backend)))
                (fp (fboundp backend-clean)))
      (funcall backend-clean))))

(defun acm-terminal-update ()
  ;; Adjust `gc-cons-threshold' to maximize temporary,
  ;; make sure Emacs not do GC when filter/sort candidates.
  (let* ((gc-cons-threshold most-positive-fixnum)
         (keyword (acm-get-input-prefix))
         (previous-select-candidate-index (+ acm-menu-offset acm-menu-index))
         (previous-select-candidate (acm-menu-index-info (acm-menu-current-candidate)))
         (candidates (acm-update-candidates))
         (menu-candidates (cl-subseq candidates 0 (min (length candidates) acm-menu-length)))
         (current-select-candidate-index (cl-position previous-select-candidate (mapcar 'acm-menu-index-info menu-candidates) :test 'equal))
         (direction (when (popon-live-p acm-terminal-menu-popon)
                      (plist-get (cdr acm-terminal-menu-popon) :direction)))
         (bounds (acm-get-input-prefix-bound)))
    (cond
     ;; Hide completion menu if user type first candidate completely.
     ((and (equal (length candidates) 1)
           (string-equal keyword (plist-get (nth 0 candidates) :label))
           ;; Volar always send back single emmet candidate, we need filter this condition.
           (not (string-equal "Emmet Abbreviation" (plist-get (nth 0 candidates) :annotation))))
      (acm-hide))
     ((> (length candidates) 0)
      (let* ((menu-old-cache (cons (acm-popup menu-max-length-cache) (acm-popup menu-number-cache))))
        ;; Enable acm-mode to inject mode keys.
        (acm-mode 1)

        ;; Use `pre-command-hook' to hide completion menu when command match `acm-continue-commands'.
        (add-hook 'pre-command-hook #'acm--pre-command nil 'local)

        ;; Adjust candidates.
        (setq-local acm-menu-offset 0)  ;init offset to 0
        (if (zerop (length acm-menu-candidates))
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

        ;; Set candidates and menu candidates.
        (setq-local acm-candidates candidates)
        (setq-local acm-menu-candidates menu-candidates)

        ;; Init colors.
        (acm-frame-init-colors)

        ;; Record menu popup position and buffer.
        (acm-popup-set 'popup-point (or (car bounds) (point)))

        ;; `posn-at-point' will failed in CI, add checker make sure CI can pass.
        ;; CI don't need popup completion menu.
        (when (posn-at-point (acm-popup popup-point))
          (acm-popup-set 'popup-position (acm-frame-get-popup-position (acm-popup popup-point)))

          ;; Create menu frame if it not exists.
          (acm-terminal-create-frame-if-not-exist acm-terminal-menu-popon acm-buffer "acm frame")
          (plist-put (cdr acm-terminal-menu-popon) :direction direction)

          ;; Render menu.
          (acm-menu-render menu-old-cache))
        ))
     (t
      (acm-hide)))))

(defvar acm-terminal-ui-overrides
  '((acm-frame-init-colors . acm-terminal-init-colors)
    (acm-hide . acm-terminal-hide)
    (acm-update . acm-terminal-update)
    (acm-doc-try-show . acm-terminal-doc-try-show)
    (acm-doc-hide . acm-terminal-doc-hide)
    (acm-doc-scroll-up . acm-terminal-doc-scroll-up)
    (acm-doc-scroll-down . acm-terminal-doc-scroll-down)
    (acm-menu-max-length . acm-terminal-max-length)
    (acm-menu-render . acm-terminal-menu-render)
    (acm-menu-render-items . acm-terminal-menu-render-items))
  "What a terminal answers instead of the child frames of acm.
This is what `acm-terminal-active' puts in `acm-ui-overrides'.  The
one thing not in it is the background the documentation is rendered
with: a terminal has no frame to take it from, so the face itself is
pointed at the colours of the terminal instead.")

(defun acm-terminal-active ()
  "Draw the acm menu with `popon' instead of the child frames acm uses.
The menu and its documentation are taken over until
`acm-terminal-deactive' is called, and `markdown-code-face' is pointed
at the colours of the terminal.  This is what a terminal needs, and
where it is done for you already."
  (unless acm-terminal--popon-available
    (user-error "The terminal menu needs popon, which is not installed"))
  (setq acm-ui-overrides acm-terminal-ui-overrides)
  (when (facep 'markdown-code-face)
    (set-face-attribute 'markdown-code-face nil
                        :background (acm-terminal-default-background))))

(defun acm-terminal-deactive ()
  "Give the acm menu, and the markdown face, back to what they were."
  (setq acm-ui-overrides nil)
  (when (facep 'markdown-code-face)
    (set-face-attribute 'markdown-code-face nil :background 'unspecified)))

;; A terminal Emacs has no child frames, so the menu is taken over there from the
;; start - but only when Emacs is interactive: in batch `window-system' is nil as
;; well, and taking acm over while the tests load it is a surprise they did not
;; ask for.
(when (and (not window-system) (not noninteractive))
  (if acm-terminal--popon-available
      (acm-terminal-active)
    (warn "The terminal acm menu is not active: popon is missing")))

(provide 'acm-terminal)

;;; acm-terminal.el ends here
