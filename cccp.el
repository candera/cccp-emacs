;; Copyright (C) 2011 Craig Andera
;;
;; Licensed under the MIT license: http://www.opensource.org/licenses/mit-license.php
;;
;; To use:
;;
;; Put the following in your emacs init file:
;;
;; (add-to-list 'load-path "/path/to/cccp-emacs-directory")
;; (require 'cccp)
;;
;; TODO: Write usage information

;;; Def true for debugging
(defvar cccp-debug-level t)
(defvar cccp-simulate-send nil)         ; Bind to t to prevent sending
                                        ; anything: only debug output
                                        ; will be produced

(defmacro cccp-debug (msg &rest args)
  `(when cccp-debug-level
     (message ,msg ,@args)))

;;; Handle buffer changes
(defvar cccp-last-before-change nil
  "Records the bit of the buffer that is about to change.

Takes the form (BEG END TEXT), where BEG is the starting position
of the imminent buffer change, END is the ending position, and
TEXT is the text of the region in question.")

(defvar cccp-edit-in-progress nil
  "True when an edit received from the agent is being processed.

We use this to suppress transmitting change notifications about
changes that have come in from other people.")

(defun cccp-compute-edits (pos buffer-size before-text after-text)
  "Compute the list of edits the current change represents.

POS is the position where the change begins.
BUFFER-SIZE is the size of the whole buffer after the change is made.
BEFORE-TEXT is the text before it was changed. This is the empty string on an insert.
AFTER-TEXT is the text after it was changed. This is the empty string on a delete."
  ;; Text property changes trigger this method, even though the text
  ;; itself hasn't changed. This can happen, e.g., during
  ;; fontification. But for purposes of collaboration, we only care
  ;; about the text.
  (unless (string= before-text after-text)
    (let ((before-length (length before-text))
          (after-length (length after-text)))
      (append
       (when (< 1 pos) (list :retain (1- pos)))
       (when (< 0 before-length) (list :delete before-text))
       (when (< 0 after-length) (list :insert after-text))
       ;; TODO: Deal with narrowing
       (let ((remaining (- buffer-size (1- pos) after-length)))
         (when (< 0 remaining)
           (list :retain remaining)))))))

(defun cccp-before-change (beg end)
  "Records the location of the change that is about to take place."
  (unless cccp-edit-in-progress
   (setq cccp-last-before-change (list beg end (buffer-substring-no-properties beg end)))))

(defun cccp-after-change (beg end len)
  "Records the location of the change that just happened."
  (unless cccp-edit-in-progress
   (let ((before-text (third cccp-last-before-change))
         (after-text (buffer-substring-no-properties beg end)))
     (cccp-debug "(beg end len) %S" (list beg end len))
     (cccp-debug "cccp-last-before-change %S" cccp-last-before-change)
     (cccp-debug "Text in buffer at position %d changed from '%s' to '%s'"
                 beg before-text after-text)
     (let ((cccp-edits (cccp-compute-edits beg (buffer-size) before-text after-text)))
       (cccp-debug "Computed changes: %S" cccp-edits)
       (when cccp-edits
         (if cccp-simulate-send
             (cccp-debug "Simulating enabled: Not actually sending to agent.")
           (cccp-agent-edit-file cccp-agent (buffer-name) cccp-edits)))))))

;;; Swank
(defun cccp-swank-length (body)
  "Calculates the length prefix that needs to go at the front of a swank message."
  (format "%06x" (length body)))

(defun cccp-swank-decode-length (input)
  "Turn the six-digit hex numeric string INPUT into a number"
  (string-to-number input 16))

(defun cccp-sexp-to-string (sexp)
  "Turn s-expression SEXP into a string suitable for transmission via swank."
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

(defun cccp-swank-decode (input &optional forms)
  "Parse INPUT and return (PARSED . REMAINING).

PARSED is a list consisting of FORMS prepended to the of objects
parsed from the input.
REMAINING in the unparseable remainder.

So, for example, '(cccp-dispatch \"000007(a b c)000007(d e f)00002f(a b\")'
would return (((a b c) (d e f)) . \"00002f(a b\""

  ;; If we don't have at least six characters (the length), there's
  ;; nothing we can do: wait for more input
  (if (> 6 (length input))
      (cons forms input)
    (let ((msg-length (cccp-swank-decode-length (substring input 0 6))))
      (if (> (+ 6 msg-length) (length input))
          ;; The whole message hasn't been received yet
          (cons forms input)
        (let ((msg-body (substring input 6 (+ 6 msg-length)))
              (remainder (substring input (+ 6 msg-length))))
          ;; Recurse to pick up any additional forms that haven't been
          ;; parsed yet
          (cccp-swank-decode remainder (append forms (list (read msg-body)))))))))

;;; Agent interaction

;; Handle incoming traffic from the agent
(defvar cccp-agent-pending-input "")

(defun cccp-insert-text (text position)
  "Insert TEXT into the current buffer at POSITION."
  (cccp-debug "Inserting %s at %d" text position)
  (save-excursion
    (goto-char position)
    (insert text)))

(defun cccp-delete-text (text position)
  "Delete TEXT from the current buffer at POSITION.

Currently, no verification is done as to whether the text at
POSITION matches TEXT: we just delete (length TEXT) characters."
  (cccp-debug "Deleting %s at %d" text position)
  (save-excursion
    (goto-char position)
    (delete-char (length text))))

;; TODO: Think of a better name for this, or for cccp-agent-edit-file
;; below, since they are confusingly similar.
(defun cccp-edit-file (edits &optional position)
  "Process a swank:edit-file command received from an agent."
  ;; Supress the modifications we're about to do from generating
  ;; modification messages, lest we go into an infinite loop and rip a
  ;; hole in the space-time continuum.
  (let ((cccp-edit-in-progress t))
   ;; TODO: somehow deal with the fact that at some point we'll be
   ;; receiving edits on multiple files
   (unless position (cccp-debug "Editing %s with %S" (buffer-name) edits))
   (when edits
     (let ((position (or position 1)))
       (case (first edits)
         (:retain
          (cccp-edit-file (cddr edits) (+ position (second edits))))
         (:insert
          (cccp-insert-text (second edits) position)
          (cccp-edit-file (cddr edits) (+ position (length (second edits)))))
         (:delete
          (cccp-delete-text (second edits) position)
          (cccp-edit-file (cddr edits) position))
         (otherwise
          (cccp-debug "Unable to process edit command %S"
                      (first edits))))))))

(defun cccp-agent-dispatch (forms)
  "Dispatches FORMS received from agent."
  (cccp-debug "Dispatching %S" forms)
  (dolist (command forms)
    ;; (swank:edit-file "foo.txt" (:retain 3 :delete "foo" :insert "bar" :retain 7))
    (if (string= :edit-performed (first command))
        (save-excursion
          (set-buffer (get-buffer (second command)))
          (cccp-edit-file (third command)))
      (cccp-debug "Unable to dispatch command %S" command))))

(defun cccp-agent-filter (agent data)
  (cccp-debug "Received data from agent: %s" data)
  (setq cccp-agent-pending-input (concat cccp-agent-pending-input data))
  (let ((parsed-input (cccp-swank-decode cccp-agent-pending-input)))
    (setq cccp-agent-pending-input (cdr parsed-input))
    (when (car parsed-input)
      (cccp-agent-dispatch (car parsed-input)))))

(defun cccp-agent-connect (port)
  "Opens a connection to the cccp agent and returns it."
  (let ((agent (open-network-stream "cccp-agent" nil "localhost" port)))
    (set-process-filter agent 'cccp-agent-filter)
    ;; TODO: cccp-agent-init-server-connection here?
    agent))

(defun cccp-send (agent sexp)
  "Sends the s-expression sexp to the cccp-agent agent."
  (let ((msg (cccp-swank-encode sexp)))
    (cccp-debug (format "Sending message: %s" msg))
    (unless cccp-simulate-send
      (process-send-string agent msg))))

(defvar cccp-swank-rpc-id 0
  "The ID of the current swank RPC.

Swank calls needs to have a unique ID. We use this variable to
record it.")

(defun cccp-next-swank-rpc-id ()
  "Return a unique ID suitable for use in a swank RPC."
  (setq cccp-swank-rpc-id (1+ cccp-swank-rpc-id)))

(defun cccp-swank-rpc (agent sexp)
  "Sends the swank command SEXP to the cccp-agent AGENT."
  (cccp-send agent (list :swank-rpc sexp (cccp-next-swank-rpc-id))))

(defun cccp-agent-disconnect (agent)
  "Closes the connection to the cccp agent"
  (delete-process agent))

(defun cccp-agent-init-server-connection (agent protocol host port)
  "Initializes the agent's connection with the server."
  (cccp-swank-rpc agent `(swank:init-connection (:protocol ,protocol :host ,host :port ,port))))

(defun cccp-agent-link-file (agent id file-name)
  "Registers changes made for the given id/file-name for synchronization via the server"
  (cccp-swank-rpc agent `(swank:link-file ,id ,file-name)))

(defun cccp-agent-unlink-file (agent file-name)
  "Deregisters changes made for the given file-name from syncrhonization via the server."
  (cccp-swank-rpc agent `(swank:unlink-file ,file-name)))

(defun cccp-agent-edit-file (agent file-name edits)
  "Sends the specified EDITS to FILE-NAME to agent AGENT.

Edits must be a list of pairs of the form TYPE VALUE, where TYPE
is one of :retain, :insert, or :delete, as specified by
https://github.com/djspiewak/cccp/. Note that the edit list must
span the entire file, even if that means having a :retain at the
end."
  (cccp-swank-rpc agent `(swank:edit-file ,file-name ,edits)))

(defun cccp-agent-shutdown (agent)
  "Sends the shutdown message to the agent."
  (cccp-swank-rpc agent `(swank:shutdown)))

(defvar cccp-agent nil
  "The agent with which this emacs is associated.")

(defvar cccp-agent-current-id 0
  "Keeps track of the current ID we're using with `cccp-agent-next-id'.")

(defun cccp-agent-next-id ()
  "Return an ID suitable for calling cccp-agent-link-file with."
  (number-to-string (setq cccp-agent-current-id (1+ cccp-agent-current-id))))

;;; Minor mode setup
(defvar cccp-mode-map (make-sparse-keymap)
  "Keymap for cccp-mode.")

(define-minor-mode cccp-mode
  "Toggle cccp-mode, a minor mode for connecting to a CCCP server.

CCCP is the Common Collaborative Coding Protocol. With no
argument, this command toggles the mode. Non-null prefix argument
turns on the mode. Null prefix argument turns off the mode.

When cccp-mode is enabled, edits in the buffer will be sent to
other colloborators via the CCCP server, and edits received from
the CCCP server will be applied to the buffer.

You probably shouldn't be turning this mode on and off yourself.
Instead, use `cccp-start-session', `cccp-link-buffer', and
`cccp-unlink-buffer'. Turning cccp-mode off will disable CCCP
from receiving change notifications, which is probably not what
you want to happen.

Note that cccp-mode is currently pre-alpha, i.e. mostly broken.
Use at your own risk."

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
    ;; (define-key cccp-mode-map (kbd "C-c /") 'cccp-display-history)

    (make-local-variable 'cccp-last-before-change)

    ;; Sign up to receive notification of changes to the buffer
    (make-local-variable 'before-change-functions)
    (make-local-variable 'after-change-functions)
    (add-to-list 'before-change-functions 'cccp-before-change)
    (add-to-list 'after-change-functions 'cccp-after-change)))

(defun cccp-link-buffer ()
  "Link the current buffer to the existing CCCP session.

Starts a new session if one has not yet been started. See `cccp-start-session'."
  (interactive)
  (message "Not yet implemented."))

(defcustom cccp-agent-path nil
  "Full path to the CCCP agent executable"
  :type 'file
  :group 'cccp)

(defun cccp-file-size (path)
  "Return the size of file at PATH."
   (nth 7 (file-attributes path)))

(defun cccp-read-port (path)
  "Return the port number in PATH."
  (save-excursion
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (read (current-buffer)))))

(defun cccp-agent-attempt-connect (host port path retries &optional attempt)
  "Try to connect to the agent whose port is listed in PATH.
Try for at most RETRIES times."
  (let ((attempt (or attempt 1)))
    (unless (active-minibuffer-window)
      (message "Polling %S%s" path (make-string (/ attempt 3) ?.)))
    (cond ((and (file-exists-p path) (> (cccp-file-size path) 0))
           (setq cccp-agent (cccp-agent-connect (cccp-read-port path)))
           (cccp-agent-init-server-connection cccp-agent "http" host port)
           (message "Agent connected!"))
          ((> retries attempt)
           (run-with-timer 0.3 nil #'cccp-agent-attempt-connect host port path retries (1+ attempt)))
          (t (message "Unable to connect to agent within specified number of retries")))))

(defun cccp-agent-launch (host port)
  "Launch a CCCP agent and connect it to the server at HOST:PORT."
  (let* ((cccp-agent-path (expand-file-name (or cccp-agent-path (read-file-name "Path to agent: "))))
         (cccp-agent-port-path (concat (file-name-as-directory temporary-file-directory)
                                       "agent.port."
                                       (number-to-string (emacs-pid)))))
    (cccp-debug "Attempting to launch %s %s" cccp-agent-path cccp-agent-port-path)
    (when (file-exists-p cccp-agent-port-path)
      (delete-file cccp-agent-port-path))
    (start-process "CCCP Agent" "*cccp-agent*" cccp-agent-path cccp-agent-port-path)
    (cccp-agent-attempt-connect host port cccp-agent-port-path 30 1)))

(defun cccp-agent-shutdown ()
  "Terminate the existing instance of the CCCP agent."
  (delete-process "CCCP Agent")
  (kill-buffer (get-buffer "*cccp-agent*"))
  (setq cccp-agent nil))

(defun cccp-start-session ()
  "Start a new CCCP collaboration session.

Launches a cccp agent instance and connects it to a server. Use
`cccp-link-buffer' to add buffers to the session."
  (interactive)
  (if cccp-agent
      (message "CCCP session has already been started.")
    (cccp-agent-launch
     (read-from-minibuffer "Server Host: " "localhost")
     (string-to-number (read-from-minibuffer "Server Port: " "8585")))))

(defun cccp-end-session ()
  "End the existing CCCP collaboration session."
  (interactive)
  (if cccp-agent
      (cccp-agent-shutdown)
    (message "There is no existing CCCP collaboration session.")))

(defun cccp-get-id ()
  "Return an ID suitable for passing to `cccp-agent-link-file'."
  (unless cccp-agent
    (error "No CCCP session has been started. Run `cccp-start-session' to create one."))
  (let ((id (read-from-minibuffer (format "Document ID (default '%s'): " (buffer-name)))))
    (if (zerop (length id))
      (buffer-name)
      id)))

(defun cccp-link-buffer (id)
  "Link the current buffer with the CCCP session under ID.

The ID is how the document will be known to the server: this is
the identifier you will use to collaborate on this buffer with
another user."
  (interactive (list (cccp-get-id)))
  (unless cccp-agent
    (error "No CCCP session has been started. Run `cccp-start-session' to create one."))
  (cccp-agent-link-file cccp-agent id (buffer-name))
  (cccp-mode t))

(defun cccp-unlink-buffer ()
  "Unlink the current buffer from the CCCP session."
  (interactive)
  (unless cccp-mode
    (error "Buffer is not linked to a CCCP session."))
  (cccp-mode nil))

(provide 'cccp)

