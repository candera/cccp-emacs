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

;;; Def true for debugging
(defvar cccp-debug-level t)
(defvar cccp-simultate-send nil)        ; Bind to t to prevent sending
                                        ; anything: only debug output
                                        ; will be produced

(defmacro cccp-debug (msg &rest args)
  `(when cccp-debug-level
     (message ,msg ,@args)))

;;; Handle buffer changes
(defvar cccp-last-before-change nil)

(defun cccp-compute-edits (pos buffer-size before-text after-text)
  "Compute the list of edits the current change represents.

POS is the position where the change begins.
BUFFER-SIZE is the size of the whole buffer after the change is made.
BEFORE-TEXT is the text before it was changed. This is the empty string on an insert.
AFTER-TEXT is the text after it was changed. This is the empty string on a delete."
  (let ((before-length (length before-text))
        (after-length (length after-text)))
    (append
     (when (< 1 pos) `(:retain ,(1- pos)))
     (when (< 0 before-length) `(:delete ,before-text))
     (when (< 0 after-length) `(:insert ,after-text))
     ;; TODO: Deal with narrowing
     ;; TODO: Is the right size to compute the one *after* all the edits?
     (let* ((length-delta (- after-length before-length))
            (remaining (- buffer-size (+ pos length-delta))))
       (when (< 0 remaining)
         (list ':retain remaining))))))

(defun cccp-before-change (beg end)
  "Records the location of the change that is about to take place."
  (setq cccp-last-before-change (list beg end (buffer-substring beg end))))

(defun cccp-after-change (beg end len)
  "Records the location of the change that just happened."
  (let ((before-text (third cccp-last-before-change))
        (after-text (buffer-substring beg end)))
    (cccp-debug "(beg end len) %S" (list beg end len))
    (cccp-debug "cccp-last-before-change %S" cccp-last-before-change)
    (cccp-debug "Text in buffer at position %d changed from '%s' to '%s'"
                beg before-text after-text)
    (cccp-debug "Computed changes: %S" (cccp-compute-edits beg (buffer-size) before-text after-text))))

;;; Swank
(defun cccp-swank-length (body)
  "Calculates the length prefix that needs to go at the front of a swank message."
  (format "%06x" (length body)))

(defun cccp-sexp-to-string (sexp)
  "Turns an s-sexpression into a string suitable for transmission via swank."
  (with-temp-buffer
    (let (print-escape-nonascii         ; Don't escape non-ASCII
          print-escape-newlines         ; Don't escape newlines
          print-length                  ; Don't truncate by length
          print-level)                  ; Don't truncate by level
      (prin1 sexp (current-buffer))
      (buffer-string))))

(defun cccp-swank-encode (sexp)
  "Encode the specified s-expression suitable for transmission via swank."
  (let ((body (concat (cccp-sexp-to-string sexp) "\n")))
    (concat (cccp-swank-length body) body)))

;;; Agent interaction

;; Handle incoming traffic from the agent
(defun cccp-agent-filter (agent data)
  (cccp-debug "Received data from agent: %s" data)
  ;; TODO: Implement
  )

(defun cccp-agent-connect (port)
  "Opens a connection to the cccp agent and returs it."
  (let ((agent (open-network-stream "cccp-agent" nil "localhost" port)))
    (set-process-filter agent 'cccp-agent-filter)
    ;; TODO: cccp-agent-init-server-connection here?
    agent))

(defun cccp-send (agent sexp)
  "Sends the s-expression sexp to the cccp-agent agent."
  (let ((msg (cccp-swank-encode sexp)))
    (cccp-debug (format "Sending message: %s" msg))
    (unless cccp-simultate-send
      (process-send-string agent msg))))

(defun cccp-agent-disconnect (agent)
  "Closes the connection to the cccp agent"
  (delete-process agent))

(defun cccp-agent-init-server-connection (agent protocol host port)
  "Initializes the agent's connection with the server."
  (cccp-send agent `(swank:init-connection (:protocol ,protocol :host ,host :port ,port))))

(defun cccp-agent-link-file (agent id file-name)
  "Registers changes made for the given id/file-name for synchronization via the server"
  (cccp-send agent `(swank:link-file ,id ,file-name)))

(defun cccp-agent-unlink-file (agent file-name)
  "Deregisters changes made for the given file-name from syncrhonization via the server."
  (cccp-send agent `(swank:unlink-file ,file-name)))

(defun cccp-agent-edit-file (agent file-name &rest edits)
  "Sends the specified edits to `file-name` to the agent.

Edits must be pairs of the form TYPE VALE, where TYPE is one
of :retain, :insert, or :delete, as specified by
https://github.com/djspiewak/cccp/. Note that the edit list must
span the entire file, even if that means having a :retain at the
end."
  (cccp-send agent `(swank:edit-file ,file-name ,edits)))


(defun cccp-agent-shutdown (agent)
  "Sends the shutdown message to the agent."
  (cccp-send agent `(swank:shutdown)))

;;; Minor mode setup
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
    (make-local-variable 'before-change-functions)
    (make-local-variable 'after-change-functions)
    (add-to-list 'before-change-functions 'cccp-before-change)
    (add-to-list 'after-change-functions 'cccp-after-change)))

(provide 'cccp-mode)