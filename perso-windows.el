;;;;;;;;;;;;;
;; require ;;
;;;;;;;;;;;;;
(require 'winner)
(require 'perso-jabber)

;;;;;;;;;;;;;;;
;; functions ;;
;;;;;;;;;;;;;;;
(defun jqt/two-third-it-up ()
  "With the current buffer and full screen window, split it in two and
make the left one eshell and 1/3 width, and the remaining 2/3 be the
current buffer.

Note that it's not really two thirds but the desired effect nonetheless.
"
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (split-window-right)
  (switch-to-buffer (other-buffer))
  (other-window -1)
  (delete-window)
  (other-window 1))

(defun jqt/toggle-window-dedication (&optional on)
  ""
  (interactive "P")
  (if on
      (set-window-dedicated-p (selected-window) t)
    (set-window-dedicated-p (selected-window) (not (window-dedicated-p (selected-window)))))
  (message "Dedicated? %s" (window-dedicated-p (selected-window))))

(defun jqt/flip-windows-buffers ()
  "Move each window's buffer to the next available window."
  (interactive)
  ())

(defun jqt/split-to-compare (&optional below)
  "Split current window to compare last two buffers.

If OTHER is nil, stay in current window, else..."
  (interactive "P P")
  (if below
      (split-window-below)
    (split-window-right))
  (other-window 1)
  (set-window-buffer (selected-window) (other-buffer)))

(defun jqt/kill-next-window (&optional num)
  "Kill next NUM window."
  (interactive "p")
  (other-window (* (jqt/unit num) 1))
  (delete-window)
  (when (and num
             (not (= (abs num) 1))
             (> (length (window-list)) 1))
    (message (format "%d" num))
    (jqt/kill-next-window (if (> num 0)
                              (1- num)
                            (1+ num)))))

(defun jqt/kill-other-frame ()
  "Kill other live frame."
  (interactive)
  (when (> (length (frame-list)) 1)
    (other-frame 1)
    (delete-frame)))

(defun jqt/continue-rotating-buffers-in-windows ()
  "Calls `jqt/rotate-buffers-in-windows' and sets up the temporary
map."
  (interactive)
  (jqt/rotate-buffers-in-windows)
  (jqt/continue 'jqt/rotate-buffers-in-windows))

(defun jqt/rotate-buffers-in-windows ()
  "Move current buffer to next window and so on such that current
  window will get the previous one's buffer."
  (interactive)
  (walk-windows
   (lambda (window)
     (set-window-buffer window (other-buffer nil))))
  (switch-to-buffer nil))

(defun jqt/continue-switching-windows (&optional counter-clockwise-p)
  "Calls `jqt/switch-windows' and sets up the temporary map."
  (interactive "P")
  (jqt/switch-windows counter-clockwise-p)
  (jqt/continue-more
   '((?f jqt/switch-windows)
     (?b '(jqt/switch-windows t)))))

(defun jqt/switch-windows (&optional counter-clockwise)
  "Convenience keybinding to switch windows."
  (interactive "P")
  (if counter-clockwise
      (other-window -1)
    (other-window 1)))

(defun jqt/last-buffer-in-previous-window ()
  "Switch to last buffer in the previous window."
  (interactive)
  (set-window-buffer (previous-window) (other-buffer)))

(defun jqt/other-frame-or-create ()
  "Switch to other frame or create."
  (interactive)
  (if (> (length (frame-list)) 1)
      (other-frame 1)
    (ido-switch-buffer-other-frame)))

(defun jqt/unit (num)
  "Returns 1 or -1 based on NUM.

E.g. if NUM is 6, then returns 1.
E.g. if NUM is -6, then returns -1"
  (/ num (abs num)))

(defun jqt/window-control ()
  "Creates a temporary overlay map for one-key window control."
  (interactive)
  (let ((map (make-sparse-keymap)))
    (jqt/define-keys map '(;; switch windows
                           (?f '(other-window 1))
                           (?b '(other-window -1))
                           ;; switch buffers
                           (?p previous-buffer)
                           (?n next-buffer)
                           (?l `(switch-to-buffer ,(other-buffer)))
                           (?j jqt/switch-to-jabber-chat-buffer)
                           ;; (?s jqt/window-control/ido-switch-buffer)
                           (?s ido-switch-buffer)
                           ;; resize windows
                           ("M-f" winner-redo)
                           ("M-b" winner-undo)
                           (?d delete-window)
                           (?D `(delete-window ,(next-window)))
                           (?c jqt/split-to-compare)
                           (?C '(jqt/split-to-compare 1))
                           (?1 delete-other-windows)
                           (?2 jqt/two-third-it-up)
                           (?3 jqt/split-window-into-three)))
    (jqt/set-temporary-overlay-map map t)))

;;;;;;;;;;;;;;
;; settings ;;
;;;;;;;;;;;;;;
(fset 'jqt/split-window-into-three
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([24 49 24 51 24 51 24 43 33554438 24 98 return 33554438 24 98 return] 0 "%d")) arg)))
(setq winner-mode 1)

;;;;;;;;;;;
;; hooks ;;
;;;;;;;;;;;
(add-hook 'org-mode-hook
          '(lambda ()
             (define-key org-mode-map [(control tab)] nil)))

;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom rules:            ;;
;; - ; for windows/buffers  ;;
;; - . for frames           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; windows
(global-set-key (kbd "C-;")     'jqt/window-control)
;; frames                       
(global-set-key (kbd "C-. o")   'jqt/other-frame-or-create)
(global-set-key (kbd "C-. k")   'jqt/kill-other-frame)



(provide 'perso-windows)