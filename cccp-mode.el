;; Copyright (C) 2011 Craig Andera
;;
;; Licensed under the MIT license: http://www.opensource.org/licenses/mit-license.php
;;
;; To use:
;;
;; Put the following in your emacs init file:
;;
;; (add-to-list 'load-path "/path/to/cccp-mode-directory")
;; (require 'cccp-mode)
;;
;; TODO: Write usage information

(defvar cccp-history-buffer nil)

(defun cccp-format-history (undo-list)
  "Formats the provided undo-list into something human-readable."
  (format "Not yet implemented: %d items in undo-list.\n" (length undo-list)))

(defun cccp-display-history ()
  "Displays the current edit history in a *cccp-history* buffer."
  (interactive)
  (let ((undo-list buffer-undo-list))
   (save-current-buffer
     (set-buffer cccp-history-buffer)
     (let ((buffer-read-only nil))
       (goto-char (point-max))
       (insert (cccp-format-history undo-list))))))

(defvar cccp-mode-map (make-sparse-keymap)
  "Keymap for cccp-mode.")

(define-minor-mode cccp-mode
  "Toggle cccp-mode.
With no argument, this command toggles the mode. Non-null prefix
argument turns on the mode. Null prefix argument turns off the
mode.

When cccp-mode is enabled, edits in the buffer will be sent to
other colloborators via the CCCP server, and edits received from
the CCCP server will be applied to the buffer.

Note tht cccp-mode is currently pre-alpha, i.e. completely
broken. Use at your own risk."

  ;; The indicator for the mode line.
  :lighter " CCCP"

  ;; The key bindings
  :keymap 'cccp-mode-map

  ;; The body

  ;; Create the results buffer
  (set (make-local-variable 'cccp-history-buffer)
       (generate-new-buffer "*cccp-history*"))
  (save-current-buffer
    (set-buffer cccp-history-buffer)
    (setq buffer-read-only t))

  ;; Set up the key bindings
  (define-key cccp-mode-map (kbd "C-c /") 'cccp-display-history)
)

(provide 'cccp-mode)