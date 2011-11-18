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

(defvar cccp-last-before-change nil)

(defun cccp-before-change (beg end)
  "Records the location of the change that is about to take place."
  (setq cccp-last-before-change (list beg end (buffer-substring beg end))))

(defun cccp-after-change (beg end len)
  "Records the location of the change that just happened."
  (let  ((after-text (buffer-substring beg end)))
    (message "Text in buffer at position %d changed from '%s' to '%s'"
             beg (third cccp-last-before-change) after-text)))

(defvar cccp-mode-map (make-sparse-keymap)
  "Keymap for cccp-mode.")

(define-minor-mode cccp-mode
  "Toggle cccp-mode, a minor mode for connecting to a CCCP server.

CCCP is the Common Collaborative Coding Protocol.
With no argument, this command toggles the mode. Non-null prefix
argument turns on the mode. Null prefix argument turns off the
mode.

When cccp-mode is enabled, edits in the buffer will be sent to
other colloborators via the CCCP server, and edits received from
the CCCP server will be applied to the buffer.

Note that cccp-mode is currently pre-alpha, i.e. completely
broken. Use at your own risk."

  ;; The indicator for the mode line.
  :lighter " CCCP"

  ;; The key bindings
  :keymap 'cccp-mode-map

  ;; If we're turning off cccp-mode, we shouldn't grab before/after
  ;; change notifications any more
  (when (not cccp-mode)
    (setq before-change-functions (remove 'cccp-before-change before-change-functions))
    (setq after-change-functions (remove 'cccp-after-change after-change-functions)))

  ;; Setup for when we're turning cccp-mode on
  (when cccp-mode
    ;; Set up the key bindings
    (define-key cccp-mode-map (kbd "C-c /") 'cccp-display-history)

    (make-local-variable 'cccp-last-before-change)

    ;; Sign up to receive notification of changes to the buffer
    (add-to-list 'before-change-functions 'cccp-before-change)
    (add-to-list 'after-change-functions 'cccp-after-change)))

(provide 'cccp-mode)