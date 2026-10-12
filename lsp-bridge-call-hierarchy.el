;;; lsp-bridge-call-hierarchy.el --- LSP bridge  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'acm-frame)

(defvar lsp-bridge-call-hierarchy-name-width 40
  "Width of the call name column in the call hierarchy buffer.")

(defvar lsp-bridge-call-hierarchy-path-width 60
  "Width of the path column in the call hierarchy buffer.")

(defvar lsp-bridge-call-hierarchy-call-max-height 30
  "Maximum height of the call hierarchy buffer.")

(defvar lsp-bridge-call-hierarchy-preview-height 30
  "Height of the preview window; 0 disables the preview.")

(defvar lsp-bridge-call-hierarchy--index 0
  "Index of the call selected in `lsp-bridge-call-hierarchy--popup-response'.")

(defvar lsp-bridge-call-hierarchy--overlay nil
  "Overlay that highlights the selected call in the popup buffer.")

(defvar lsp-bridge-call-hierarchy--popup-response nil
  "List of calls currently shown in the call hierarchy popup.")

(defvar lsp-bridge-call-hierarchy--temp-buffers nil
  "Buffers opened during preview that didn't exist before peeking.
They are killed when the call hierarchy popup is quit.")

(defvar lsp-bridge-call-hierarchy--local-modelines nil
  "Alist of buffer and saved mode line for previewed buffers.
It is used to restore `mode-line-format' when the popup is quit.")

(defvar lsp-bridge-call-hierarchy--origin-window nil
  "Window that was selected before the call hierarchy popup opened.")

(defvar lsp-bridge-call-hierarchy--preview-window nil
  "Window showing the preview of the selected call.")

;;;###autoload
(defun lsp-bridge-incoming-call-hierarchy ()
  "Show all symbols that call the symbol at point."
  (interactive)
  (lsp-bridge-call-file-api "prepare_call_hierarchy_incoming"
                            (lsp-bridge--position)))

;;;###autoload
(defun lsp-bridge-outgoing-call-hierarchy ()
  "Show all symbols called by the symbol at point."
  (interactive)
  (lsp-bridge-call-file-api "prepare_call_hierarchy_outgoing"
                            (lsp-bridge--position)))


(defun lsp-bridge-call-hierarchy-make-frame (buffer)
  "Create or reuse the call hierarchy frame showing BUFFER.
The frame height is the preview height plus the number of calls
capped at `lsp-bridge-call-hierarchy-call-max-height', and its
width covers the icon and the name and path columns."
  (let* ((height (+ lsp-bridge-call-hierarchy-preview-height
                    (min (length lsp-bridge-call-hierarchy--popup-response)
                         lsp-bridge-call-hierarchy-call-max-height)))
         (width (+ 6 ;; length of spaces and icon
                   lsp-bridge-call-hierarchy-name-width
                   lsp-bridge-call-hierarchy-path-width)))
    (acm-frame-create-frame-if-not-exist call-hierarchy-frame buffer
                                         "call hierarchy" 0 nil)
    (acm-frame-set-frame-size (acm-popup call-hierarchy-frame) width height)
    (acm-frame-set-frame-colors (acm-popup call-hierarchy-frame))
    (acm-frame-set-frame-pos-center (acm-popup call-hierarchy-frame))))

(defun lsp-bridge-call-hierarchy-format-call-string (s l)
  "Return string S padded or truncated to width L."
  (if (> (length s) l)
      (format "%s%s" (substring s 0 (- l (length "..."))) "...")
    (concat s (make-string (- l (length s)) ?\s))))

(defun lsp-bridge-call-hierarchy-format-call (call)
  "Return the display string for the call plist CALL.
CALL provides the keys :name, :rel_path and :icon."
  (let ((name (plist-get call :name))
        (path (plist-get call :rel_path))
        (icon (cdr (assoc (plist-get call :icon) acm-icon-alist))))
    (format "%s %s %s\n"
            (if icon
                (apply #' acm-icon-build icon)
             " ")
            (lsp-bridge-call-hierarchy-format-call-string name lsp-bridge-call-hierarchy-name-width)
            (lsp-bridge-call-hierarchy-format-call-string path lsp-bridge-call-hierarchy-path-width))))

(defvar-local lsp-bridge-call-hierarchy-overrides nil
  "Alist of command to the function that takes it over in this buffer.
A popup that is driven by the same keys but shows something else - the code
action menu is one - puts an entry here instead of advising the command, so the
command keeps its own definition and no other file has to be touched.")

(defun lsp-bridge-call-hierarchy--override (command)
  "Return the function taking COMMAND over in this buffer, or nil."
  (cdr (assq command lsp-bridge-call-hierarchy-overrides)))

(defun lsp-bridge-call-hierarchy-maybe-preview ()
  "Preview the call selected in the call hierarchy popup.
Do nothing when `lsp-bridge-call-hierarchy-preview-height' is 0.
This switches to the preview window, shows the call and hides the
mode line of the previewed buffer.  Whatever
`lsp-bridge-call-hierarchy-overrides' says takes the command over here is
called instead."
  (let ((override (lsp-bridge-call-hierarchy--override 'lsp-bridge-call-hierarchy-maybe-preview)))
    (if override
        (funcall override)
      (when (> lsp-bridge-call-hierarchy-preview-height 0)
        (other-window 1)
        (lsp-bridge-call-hierarchy-show)
        (recenter)

        (if (and mode-line-format
                 (not (member (current-buffer) lsp-bridge-call-hierarchy--temp-buffers)))
            (add-to-list 'lsp-bridge-call-hierarchy--local-modelines
                         (list (current-buffer) mode-line-format)))

        (setq-local mode-line-format nil)
        (other-window -1)))))

(defun lsp-bridge-call-hierarchy--popup (response)
  "Show the call hierarchy popup for RESPONSE.
RESPONSE is the list of call plists to display.  This creates the
frame and its buffers, splits off the preview window when preview
is enabled and enables `lsp-bridge-call-hierarchy-mode'."
  (interactive)
  (setq lsp-bridge-call-hierarchy--popup-response response)
  (setq lsp-bridge-call-hierarchy--origin-window (selected-window))
  (lsp-bridge-call-hierarchy-make-frame (get-buffer-create "*lsp-bridge-call-hierarchy*"))

  (with-current-buffer "*lsp-bridge-call-hierarchy*"
    (dolist (call lsp-bridge-call-hierarchy--popup-response)
      (insert (lsp-bridge-call-hierarchy-format-call call)))
    (delete-backward-char 1))

  (when (> lsp-bridge-call-hierarchy-preview-height 0)
    (split-window (selected-window) nil 'down)
    (other-window 1)
    (setq lsp-bridge-call-hierarchy--preview-window (selected-window))
    (set-window-text-height (selected-window)  (1- lsp-bridge-call-hierarchy-preview-height))
    (other-window -1))

  (setq lsp-bridge-call-hierarchy--temp-buffers nil)
  (setq lsp-bridge-call-hierarchy--index 0)

  (lsp-bridge-call-hierarchy-maybe-preview)

  (lsp-bridge-call-hierarchy-mode)
  (with-current-buffer "*lsp-bridge-call-hierarchy*"
    (setq-local cursor-type nil)
    (setq-local truncate-lines t)
    (setq-local mode-line-format nil)))

(defun lsp-bridge-call-hierarchy--move (next)
  "Move the call selection NEXT lines down.
Do nothing when the new index would fall outside
`lsp-bridge-call-hierarchy--popup-response'.  This moves point and
the highlight overlay and updates the preview."
  (when (and (>= (+ lsp-bridge-call-hierarchy--index next) 0)
             (< (+ lsp-bridge-call-hierarchy--index next)
                (length lsp-bridge-call-hierarchy--popup-response)))
      (forward-line next)
      (move-overlay lsp-bridge-call-hierarchy--overlay
                    (line-beginning-position) (1+ (line-end-position)))
      (setq lsp-bridge-call-hierarchy--index
            (+ next lsp-bridge-call-hierarchy--index))
      (lsp-bridge-call-hierarchy-maybe-preview)))

;;;###autoload
(defun lsp-bridge-call-hierarchy-next ()
  "Select the next call in the call hierarchy buffer."
  (interactive)
  (lsp-bridge-call-hierarchy--move 1))

;;;###autoload
(defun lsp-bridge-call-hierarchy-prev ()
  "Select the previous call in the call hierarchy buffer."
  (interactive)
  (lsp-bridge-call-hierarchy--move -1))

;;;###autoload
(defun lsp-bridge-call-hierarchy-preview-next ()
  "Scroll the call hierarchy preview window up five lines."
  (interactive)
  (with-selected-window (get-buffer-window "*lsp-bridge-code-action-preview*")
    (scroll-up-line 5)))

;;;###autoload
(defun lsp-bridge-call-hierarchy-preview-prev ()
  "Scroll the call hierarchy preview window down five lines."
  (interactive)
  (with-selected-window (get-buffer-window "*lsp-bridge-code-action-preview*")
    (scroll-down-line 5)))

;;;###autoload
(defun lsp-bridge-call-hierarchy-scroll-left ()
  "Scroll the call hierarchy preview window left five columns."
  (interactive)
  (with-selected-window (get-buffer-window "*lsp-bridge-code-action-preview*")
    (scroll-left 5)))

;;;###autoload
(defun lsp-bridge-call-hierarchy-scroll-right ()
  "Scroll the call hierarchy preview window right five columns."
  (interactive)
  (with-selected-window (get-buffer-window "*lsp-bridge-code-action-preview*")
    (scroll-right 5)))

;;;###autoload
(defun lsp-bridge-call-hierarchy-quit ()
  "Quit the call hierarchy popup and restore the previous window layout.
This deletes the frame and its buffer, deletes the preview window,
restores the saved mode lines, kills the temporary buffers opened
for preview and forgets the saved origin and preview windows."
  (interactive)
  (let ((origin-window lsp-bridge-call-hierarchy--origin-window)
        (preview-window lsp-bridge-call-hierarchy--preview-window))
    (acm-frame-delete-frame call-hierarchy-frame)
    (when (get-buffer "*lsp-bridge-call-hierarchy*")
      (kill-buffer "*lsp-bridge-call-hierarchy*"))

    (when (window-live-p preview-window)
      (delete-window preview-window))

    (when (window-live-p origin-window)
      (select-window origin-window)
      (select-frame-set-input-focus (window-frame origin-window))))

  (dolist (buffer-modeline lsp-bridge-call-hierarchy--local-modelines)
    (with-current-buffer (car buffer-modeline)
      (setq-local mode-line-format (cdr buffer-modeline))))

  (dolist (buffer lsp-bridge-call-hierarchy--temp-buffers)
    (kill-buffer buffer))

  (setq lsp-bridge-call-hierarchy--origin-window nil)
  (setq lsp-bridge-call-hierarchy--preview-window nil))

(defun lsp-bridge-call-hierarchy-show ()
  "Show the call selected in the call hierarchy buffer.
This visits the file of the selected call, moves point to the
start of its range and pulses the region of the call.  Files that
didn't exist before are remembered in
`lsp-bridge-call-hierarchy--temp-buffers'."
  (let* ((call  (nth lsp-bridge-call-hierarchy--index lsp-bridge-call-hierarchy--popup-response))
         (path (lsp-bridge-pick-file-path (plist-get call :path)))
         (range-start (plist-get (plist-get call :range) :start))
         (range-end (plist-get (plist-get call :range) :end))
         (sline (plist-get range-start :line))
         (scharacter (plist-get range-start :character))
         (eline (plist-get range-end :line))
         (echaracter (plist-get range-end :character))
         (found-buffer))

    (if (dolist (buffer (buffer-list) found-buffer)
          (when (lsp-bridge-path-equal (buffer-file-name buffer) path)
            (setq found-buffer buffer)))
        (find-file path)
      (add-to-list 'lsp-bridge-call-hierarchy--temp-buffers (find-file path)))

    (goto-char (point-min))
    (forward-line sline)
    (goto-char (+ (point-at-bol) scharacter))
    (if (equal sline eline)
        (pulse-momentary-highlight-region (point)
                                          (+ (point) (- echaracter scharacter)))
      (pulse-momentary-highlight-region (point)
                                        (save-excursion
                                          (forward-line (- eline sline))
                                          (goto-char (+ (point-at-bol) echaracter))
                                          (point))))))

;;;###autoload
(defun lsp-bridge-call-hierarchy-select ()
  "Jump to the selected call and quit the call hierarchy popup."
  (interactive)
  (lsp-bridge-call-hierarchy-quit)
  (lsp-bridge-call-hierarchy-show))

(defvar lsp-bridge-call-hierarchy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j") 'lsp-bridge-call-hierarchy-next)
    (define-key map (kbd "k") 'lsp-bridge-call-hierarchy-prev)
    (define-key map (kbd "C-n") 'lsp-bridge-call-hierarchy-next)
    (define-key map (kbd "C-p") 'lsp-bridge-call-hierarchy-prev)
    (define-key map (kbd "<down>") 'lsp-bridge-call-hierarchy-next)
    (define-key map (kbd "<up>") 'lsp-bridge-call-hierarchy-prev)
    (define-key map (kbd "M-n") 'lsp-bridge-call-hierarchy-preview-next)
    (define-key map (kbd "M-p") 'lsp-bridge-call-hierarchy-preview-prev)
    (define-key map (kbd "M-b") #'lsp-bridge-call-hierarchy-scroll-right)
    (define-key map (kbd "M-f") #'lsp-bridge-call-hierarchy-scroll-left)
    (define-key map (kbd "m") 'lsp-bridge-call-hierarchy-select)
    (define-key map (kbd "RET") 'lsp-bridge-call-hierarchy-select)
    (define-key map (kbd "<escape>") 'lsp-bridge-call-hierarchy-quit)
    (define-key map (kbd "q") 'lsp-bridge-call-hierarchy-quit)
    (define-key map (kbd "C-g") 'lsp-bridge-call-hierarchy-quit)
    map)
  "Keymap used in `lsp-bridge-call-hierarchy-mode'.")

(define-derived-mode lsp-bridge-call-hierarchy-mode text-mode "call hierarchy"
  "Major mode for browsing the call hierarchy popup buffer.
This is derived from `text-mode' and makes the buffer read-only.
It highlights the selected call with
`lsp-bridge-font-lock-flash' and installs
`lsp-bridge-call-hierarchy-mode-map'."
  ;; Kill all local variables.
  (kill-all-local-variables)
  ;; Switch new mode.
  (setq major-mode 'lsp-bridge-call-hierarchy-mode)
  (setq mode-name "call hierarchy")
  (goto-char (1+ (point-min)))
  (setq lsp-bridge-call-hierarchy--overlay (make-overlay (line-beginning-position)
                                                         (1+ (line-end-position))))
  ;; :extend extend face after EOL
  (set-face-attribute 'lsp-bridge-font-lock-flash (selected-frame) :extend t)
  (overlay-put lsp-bridge-call-hierarchy--overlay 'face 'lsp-bridge-font-lock-flash)
  (setq lsp-bridge-call-hierarchy--index 0)

  ;; Injection keymap.
  (use-local-map lsp-bridge-call-hierarchy-mode-map)
  (when (featurep 'evil)
    (evil-set-initial-state 'lsp-bridge-call-hierarchy-mode 'emacs))
  (read-only-mode 1))

(provide 'lsp-bridge-call-hierarchy)
;;; lsp-bridge-call-hierarchy.el ends here
