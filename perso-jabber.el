;;;;;;;;;;;;;
;; require ;;
;;;;;;;;;;;;;
(require 'jabber)

;;;;;;;;;;;;;;
;; settings ;;
;;;;;;;;;;;;;;
(setq jabber-account-list
      '(("jerometruong@gmail.com"
         (:password . nil)
         (:network-server . "talk.google.com")
         (:port . 443)
         (:connection-type . ssl))))

;;;;;;;;;;;;;;;
;; functions ;;
;;;;;;;;;;;;;;;
(defun jqt/cycle-jabber-chat-buffers ()
  "Sets temporary map for fast jabber chat buffer switching."
  (interactive)
  (jqt/switch-to-jabber-chat-buffer)
  (jqt/continue 'jqt/switch-to-jabber-chat-buffer))

(defun jqt/switch-to-jabber-chat-buffer ()
  "Switch or pop to the jabber chat buffer.
If a jabber window is already selected, then switch to the next chat
  buffer.

Caveats:

 - if a chat window is opened but the containing buffer is the
  next chat, it'll pop a new window.  This happened once possibly due
  to getting the ordered buffer list in a bad state while testing this
  method."
  (interactive)
  (let* ((next-chat-buffer (jqt/next-jabber-chat-buffer))
         (jabber-chat-window (when next-chat-buffer
                               (get-buffer-window next-chat-buffer))))
    (if jabber-chat-window
        (if (eq jabber-chat-window (selected-window))
            (progn
              (bury-buffer next-chat-buffer)
              (set-window-buffer jabber-chat-window (jqt/next-jabber-chat-buffer)))
          (select-window jabber-chat-window))
      (pop-to-buffer next-chat-buffer))))

(defun jqt/next-jabber-chat-buffer (&optional i)
  "Filters buffer list for names that start with `*-jabber-chat-'."
  (elt
   (remove-if-not
    (lambda (buffer) (string-match "^\*-jabber-chat-" (buffer-name buffer)))
    (buffer-list))
   (or i 0)))

;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;

;;;;;;;;;
;; end ;;
;;;;;;;;;
(provide 'perso-jabber)
