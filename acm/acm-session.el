;;; acm-session.el --- Per-frame popup state of the completion menu -*- lexical-binding: t -*-

;;; Commentary:

;; Everything a popup owns - the child frames, the buffers they display, where
;; the menu was popped up, the caches that keep the menu from recalculating its
;; size, and whether the documentation frame was asked to stay hidden - belongs
;; to the parent frame the popup was created for rather than to the process.
;; Two Emacs frames (one per macOS Space, or one per display) each show their
;; own popup, and the state of one must not leak into the other.  That is what
;; the globals this file replaces got wrong: the menu used to be a single set of
;; frames and buffers, so it deleted them as soon as another frame became
;; selected, which is also what made a popup appear on the wrong Space.
;;
;; An `acm-session' structure holds that state for one parent frame and lives in
;; `acm-sessions', a hash table keyed with `eq' - by a frame at run time, by
;; anything else in the tests, where a second real frame cannot be created.
;; `acm-popup' reads and writes one slot of the session of the current frame (or
;; of a frame given to it) and is a `setf' place, so the menu code keeps reading
;; like it did when these were variables.  `acm-session-drop' throws a session
;; away, frames and buffers included; it is hooked to `delete-frame-functions'
;; so that a session cannot outlive its parent.

;;; Code:

(require 'cl-lib)

(cl-defstruct acm-session
  "The popup state that belongs to one parent frame.

Slots:
  menu-frame   The child frame showing the candidates, or nil when the menu
               has not been shown in this frame yet.
  doc-frame    The child frame showing the documentation of the candidate.
  menu-buffer  The buffer the menu frame displays; the candidates of the
               buffer being completed are rendered into it.
  doc-buffer   The buffer the documentation frame displays.
  popup-point  Where the menu was popped up in the source buffer, which is
               also where a candidate is inserted when it is completed.
  popup-position  Cons of the popup coordinates, adjusted while the menu grows
               or while a preview changes its height.
  menu-max-length-cache  Widest candidate line of the last render, kept so
               that the menu size is not recalculated on every keystroke.
  menu-number-cache  Number of candidates the last render showed.
  doc-hide-p   Non-nil when the last update asked the documentation frame to
               stay hidden.
  markdown-doc  The documentation string the doc buffer was rendered from, so
               that the same text is not rendered again.
  markdown-timer  Timer of a pending markdown render of the doc buffer.
  call-hierarchy-frame  The child frame showing the call hierarchy of the
               symbol at point, which is per frame for the same reason.
  hover-frame  The child frame showing the hover documentation of the symbol at
               point, which lsp-bridge itself owns (unlike `doc-frame', which
               belongs to the acm backend).
  signature-frame  The child frame showing the signature help of the call at
               point."
  (menu-frame nil)
  (doc-frame nil)
  (menu-buffer nil)
  (doc-buffer nil)
  (popup-point nil)
  (popup-position nil)
  (menu-max-length-cache 0)
  (menu-number-cache 0)
  (doc-hide-p nil)
  (markdown-doc nil)
  (markdown-timer nil)
  (call-hierarchy-frame nil)
  (hover-frame nil)
  (signature-frame nil))

(defvar acm-sessions (make-hash-table :test 'eq)
  "The popup sessions, keyed by the frame their popup belongs to.
The key is a frame at run time; anything `eq'-comparable works, which is what
the tests use to stand in for the second frame they cannot create in batch.")

(defun acm-session--frame ()
  "Return the frame the popup being built belongs to.
That is the frame of the window showing the current buffer - so that completing
in a window of another frame builds that frame's popup - unless that window is
one of the popups itself, in which case its parent frame owns the session."
  (let* ((window (or (get-buffer-window (current-buffer))
                     (selected-window)))
         (frame (window-frame window)))
    (or (frame-parent frame) frame)))

(defun acm-session (&optional frame)
  "Return the popup session of FRAME, creating it when it does not exist.
FRAME defaults to `acm-session--frame'."
  (let ((frame (or frame (acm-session--frame))))
    (or (gethash frame acm-sessions)
        (puthash frame (make-acm-session
                        :menu-buffer (generate-new-buffer " *acm-buffer*")
                        :doc-buffer (generate-new-buffer " *acm-doc-buffer*"))
                 acm-sessions))))

(defun acm-session-drop (&optional frame)
  "Throw the popup session of FRAME away, frames and buffers included.
Every child frame of the session is deleted - the menu, the documentation, the
call hierarchy, the hover documentation and the signature help - and so are the
menu and documentation buffers.  FRAME defaults to the frame of the current
buffer.  Nothing happens when FRAME has no session."
  (let* ((frame (or frame (acm-session--frame)))
         (session (gethash frame acm-sessions)))
    (when session
      (dolist (child (list (acm-session-menu-frame session)
                           (acm-session-doc-frame session)
                           (acm-session-call-hierarchy-frame session)
                           (acm-session-hover-frame session)
                           (acm-session-signature-frame session)))
        (when (frame-live-p child)
          (delete-frame child)))
      (setf (acm-session-menu-frame session) nil
            (acm-session-doc-frame session) nil
            (acm-session-call-hierarchy-frame session) nil
            (acm-session-hover-frame session) nil
            (acm-session-signature-frame session) nil)
      (dolist (buffer (list (acm-session-menu-buffer session)
                            (acm-session-doc-buffer session)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))
      (remhash frame acm-sessions))))

(defun acm-session-all ()
  "Return the popup sessions of every frame, as a list.
Used where a change - a new theme, for one - has to reach every popup rather
than only the one of the current frame."
  (let (sessions)
    (maphash (lambda (_frame session) (push session sessions)) acm-sessions)
    sessions))

(defun acm-session-popup-buffer-p (&optional buffer)
  "Return non-nil when BUFFER, or the current buffer, is a popup buffer.
The buffers of every session are checked, so a caller can tell whether it is
running inside the menu or the documentation frame."
  (let ((buffer (or buffer (current-buffer))))
    (cl-some (lambda (session)
               (memq buffer (list (acm-session-menu-buffer session)
                                  (acm-session-doc-buffer session))))
             (acm-session-all))))

(defmacro acm-popup (slot &optional frame)
  "Return the popup state SLOT of the session of FRAME.
SLOT is a slot of `acm-session', FRAME defaults to the frame of the current
buffer.  The place can be stored with `setf', so the menu code reads like it
did when this state lived in global variables."
  `(,(intern (format "acm-session-%s" (symbol-name slot))) (acm-session ,frame)))

(defun acm-popup-set (slot value &optional frame)
  "Store VALUE in the popup state SLOT of the session of FRAME.
SLOT is a symbol naming a slot of `acm-session' and FRAME defaults to the frame
of the current buffer.  This is the writing counterpart of the `acm-popup'
macro: the slot it reads is a literal, which a `setf' expander cannot keep."
  (setf (cl-struct-slot-value 'acm-session slot (acm-session frame)) value))

(defun acm-session-delete-frame-hook (frame)
  "Drop the popup session of FRAME, which is about to be deleted."
  (acm-session-drop frame))

(add-hook 'delete-frame-functions #'acm-session-delete-frame-hook)

(provide 'acm-session)

;;; acm-session.el ends here
