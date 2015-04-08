;;;;;;;;;;;;;
;; require ;;
;;;;;;;;;;;;;
(require 'winner)
(require 'perso-jabber)

;;;;;;;;;;;;;;;
;; variables ;;
;;;;;;;;;;;;;;;
(defgroup perso/windows nil
  "Personal windows variables.")

(defcustom window-control-map '(;; switch windows
                                (?f '(other-window 1))
                                (?b '(other-window -1))
                                ;; switch buffers
                                (?p previous-buffer)
                                (?n next-buffer)
                                (?l `(switch-to-buffer ,(other-buffer)))
                                (?j jqt/switch-to-jabber-chat-buffer)
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
                                (?3 jqt/split-window-into-three))
  ""
  :type 'list
  :group 'perso/windows)

(defvar jqt/window-control-p nil
  "")

;;;;;;;;;;;;;;
;; settings ;;
;;;;;;;;;;;;;;
(winner-mode 1)

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
  (split-window-right)
  (split-window-right)
  (balance-windows)
  (other-window 2)
  (delete-window)
  (set-window-buffer (selected-window) (other-buffer))
  (other-window 1))

(defun jqt/toggle-window-dedication (&optional on)
  ""
  (interactive "P")
  (if on
      (set-window-dedicated-p (selected-window) t)
    (set-window-dedicated-p (selected-window) (not (window-dedicated-p (selected-window)))))
  (message "Dedicated? %s" (window-dedicated-p (selected-window))))

(defun jqt/split-to-compare (&optional belowp)
  "Split current window to compare last two buffers.

TODO If OTHER is nil, stay in current window, else..."
  (interactive "P P")
  (if belowp
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

(defun jqt/switch-buffer-with-window (&optional next-window-p)
  "Switch this window's buffer with the previous window by default, or
the next one if `NEXT-WINDOW-P'.  It does nothing if the `OTHER-WINDOW'
is dedicated."
  (interactive "P")
  (let* ((buffer (current-buffer))
         (other-window (if next-window-p
                           (next-window)
                         (previous-window)))
         (other-buffer (window-buffer other-window)))
    (unless (window-dedicated-p other-window)
      (switch-to-buffer other-buffer)
      (set-window-buffer other-window buffer))))

(defun jqt/window-control ()
  "Creates a temporary overlay map for one-key window control."
  (interactive)
  (setq jqt/window-control-p t)
  (let ((map (make-sparse-keymap)))
    ;; TODO: defcustom this map.
    (jqt/define-keys map '(;; switch windows
                           (?f '(other-window 1))
                           (?b '(other-window -1))
                           (?s jqt/switch-buffer-with-window)
                           ;; TODO missing register ("r w" window-configuration-to-register)
                           ;; TODO ("r j" jump-to-register)

                           ;; switch frames
                           (?o '(other-frame 1))

                           ;; switch buffers
                           ;; TODO move these to their own buffer map
                           ;; so keys can be reused.
                           (?p previous-buffer)
                           (?n next-buffer)
                           (?l `(switch-to-buffer ,(other-buffer)))
                           (?j jqt/switch-to-jabber-chat-buffer)
                           (?J '(jqt/switch-to-jabber-chat-buffer 1))
                           (?' shell-switcher-switch-buffer-other-window)
                           (?m perso/windows/mark-window)
                           ;; TODO unmark buffer
                           (?U perso/windows/clear-marked-windows)
                           (?B perso/windows/cycle-marked-windows)
                           
                           ;; window layout
                           (?> winner-redo)
                           (?< winner-undo)
                           (?d delete-window)
                           (?D `(delete-window ,(next-window)))
                           (?c jqt/split-to-compare)
                           (?C '(jqt/split-to-compare 1))
                           (?1 delete-other-windows)
                           (?2 split-window-below)
                           (?@ '(jqt/split-to-compare 1))
                           (?3 split-window-right)
                           (?# perso/windows/lickity-split)
                           (?! jqt/toggle-window-dedication)
                           ;; normally i use a DELTA of 10 when i run
                           ;; the command manually and wondering if
                           ;; it's because i have the font set much
                           ;; lower (maybe twice lower) that i only
                           ;; need to use a DELTA of 5 here.
                           (?- (enlarge-window -5))
                           (?+ (enlarge-window 5))))
    (jqt/set-temporary-overlay-map map t)))

(defvar perso/windows/windows nil)
(defun perso/windows/mark-window ()
  "Mark by collecting window into `perso/windows/windows'."
  (interactive)
  (let ((windows perso/windows/windows)
        (new (selected-window)))
    (setq perso/windows/windows (remove-duplicates (append windows (list new))))
    (message "Marked windows: %s." perso/windows/windows)))

(defun perso/windows/clear-marked-windows ()
  "Resets `perso/windows/windows."
  (interactive)
  (setq perso/windows/windows nil)
  (message "Cleared marked windows."))

(defun perso/windows/cycle-marked-windows ()
  "Switch to next window in `perso/windows/windows'."
  (interactive)
  (let* ((windows perso/windows/windows)
         (next (car windows))
         (len  (1- (length windows)))
         (rest (last windows len)))
    (setq perso/windows/windows (append rest (list next)))
    (if (eq next (selected-window))
        (message "Currently on the next buffer in the cycle. Fixed it, try again.")
        (select-window next))))

(fset 'jqt/split-window-into-three
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([24 49 24 51 24 51 24 43 33554438 24 98 return 33554438 24 98 return] 0 "%d")) arg)))

(defun perso/windows/lickity-split (&optional delta)
  (interactive "P")
  (delete-other-windows)
  (let ((delta (or delta 20)))
    (split-window-below)
    (enlarge-window delta)
    (split-window-right)
    (set-window-buffer (next-window) (other-buffer))
    (set-window-buffer (next-window (next-window)) (other-buffer))))

(defun perso/windows/cycle ()
  (interactive)
  (jqt/switch-buffer-with-window)
  (other-window 1)
  (jqt/switch-buffer-with-window)
  (other-window 1))

(defun perso/windows/popup-last-buffer-with-regexp (regexp &optional height)
  "Create a new window with the last buffer matching `REGEXP'.

New window spans the frame's width and is set to `HEIGHT' or a
third of the frame's height by default."
  (interactive "sRegexp: \ndDelta: ")
  ;; make the window arrangements
  (switch-to-buffer (jqt/next-buffer-with-regexp regexp)))

(defun perso/windows/popup-last-org ()
  (interactive)
  (perso/popup-last-buffer-with-regexp "\\.org$"))

;;;;;;;;;;;;;
;; advices ;;
;;;;;;;;;;;;;


;;;;;;;;;;;
;; hooks ;;
;;;;;;;;;;;


;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom rules:            ;;
;; - ; for windows/buffers  ;;
;; - . for frames           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; windows
(global-set-key (kbd "C-M-;")     'jqt/window-control)



(provide 'perso-windows)
