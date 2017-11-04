;;;;;;;;;;;;;
;; require ;;
;;;;;;;;;;;;;
(require 'jabber)

;;; Protected accounted in ~/.authinfo.gpg
(setq jabber-account-list
      `(("jerometruong@gmail.com"
         (:network-server . "talk.google.com")
         (:port . 5223)
         (:connection-type . ssl))))

;;;;;;;;;;;;;;;
;; functions ;;
;;;;;;;;;;;;;;;
(defun jqt/cycle-jabber-chat-buffers ()
  "Sets temporary map for fast jabber chat buffer switching."
  (interactive)
  (jqt/switch-to-jabber-chat-buffer)
  (jqt/continue 'jqt/switch-to-jabber-chat-buffer))

(defun jqt/switch-to-jabber-chat-buffer (&optional switch-p)
  "Switch or pop to the jabber chat buffer.
If a jabber window is already selected, then switch to the next chat
  buffer.

Caveats:

- if a chat window is opened but the containing buffer is the
  next chat, it'll pop a new window.  This happens when killing a
  chat buffer."
  (interactive)
  (let* ((bname-regexp "^\*-jabber-chat-")
         (next-chat-buffer (jqt/next-buffer-with-regexp bname-regexp))
         (jabber-chat-window (when next-chat-buffer
                               (get-buffer-window next-chat-buffer))))
    (jqt/next-buffer-window-condition next-chat-buffer
                                      jabber-chat-window
                                      `(jqt/next-buffer-with-regexp ,bname-regexp)
                                      switch-p)))

(defun jqt/next-jabber-chat-buffer (&optional i)
  "Filters buffer list for names that start with `*-jabber-chat-'."
  (elt
   (remove-if-not
    (lambda (buffer) (string-match "^\*-jabber-chat-" (buffer-name buffer)))
    (buffer-list))
   (or i 0)))

(defun perso/jabber/display-roster ()
  ""
  (interactive)
  (jabber-display-roster)               ; set it up before display
  (set-window-buffer (selected-window) jabber-roster-buffer))

;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;
;;; overrides
;;; default key C-x C-j is stolen by dired-jump
(global-set-key (kbd "C-c j c") 'jabber-connect-all)
(global-set-key (kbd "C-c j d") 'jabber-disconnect)
(global-set-key (kbd "C-c j r") 'perso/jabber/display-roster)
;;; custom
(global-set-key (kbd "C-c j j") 'jqt/switch-to-jabber-chat-buffer)
(global-set-key (kbd "C-c j J") (lambda () (interactive) (jqt/switch-to-jabber-chat-buffer 1)))


;;;;;;;;;
;; end ;;
;;;;;;;;;
(provide 'perso-jabber)
