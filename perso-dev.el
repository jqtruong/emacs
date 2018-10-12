(provide 'perso-dev)

(defun perso/dev/change-color (hex amount)
  "Change a color string in hex by a decimal amount, so the hex
is first converted to its rgb decimal counterparts."
  (interactive (list (thing-at-point 'symbol)
                     (string-to-number (read-from-minibuffer "Amount: "))))  
  (let* ((rgb (perso/dev/list-rgb-of-hex hex))
         (bounds (bounds-of-thing-at-point 'symbol))
         (new-hex (loop
                     for c in rgb
                     append
                       (let* ((a (+ amount (cdr c)))
                              (b (cond ((< a 0) 0)
                                       ((> a 255) 255)
                                       (t a)))
                              (c (format "%x" b)))
                         (list (if (< (length c) 2) (concat "0" c) c)))
                     into new-hex
                     finally return (mapconcat 'identity new-hex ""))))
    (kill-region (car bounds) (cdr bounds))
    (insert new-hex)))

(defun perso/dev/short-to-full-hex (hex)
  (cond
    ((= 3 (length hex))
     (loop
        for c in (string-to-list hex)
        for d = (make-string 2 c) then (concat d (make-string 2 c))
        do (message "%s" d)
        finally return d))
    (t hex)))

(defun perso/dev/list-rgb-of-hex (hex)
  "Returns `RGB' in decimal."
  (interactive (list (thing-at-point 'symbol)))
  (mapcar* 'cons
           '(:r :g :b)
           (loop
              for c across (perso/dev/short-to-full-hex hex)
              for cc = (make-string 1 c) then
                (if (= 2 (length cc))
                    (make-string 1 c)
                  (concat cc (make-string 1 c)))
              when (= 2 (length cc)) collect (string-to-number cc 16))))

(defun perso/dev/kill-rgb-of-hex (hex)
  (interactive (list (thing-at-point 'symbol)))
  (let* ((rgb (perso/dev/list-rgb-of-hex hex))
         (r   (cdr (assoc :r rgb)))
         (g   (cdr (assoc :g rgb)))
         (b   (cdr (assoc :b rgb))))
    (kill-new (format "%s, %s, %s" r g b))))

(defun perso/dev/refresh-chromium (tab)
  (interactive "p")
  (start-process "perso" nil "refresh-browser" "Chromium" (number-to-string tab)))

(defun perso/dev/relchrothemax (&optional go-back-p)
  ""
  (interactive "P")
  (save-buffer)
  (message "done %s" go-back-p)
  (start-process "perso" nil "relchrothemax" (if go-back-p "1" "")))

(defun perso/dev/save-and-go (tab)
  (interactive "p")
  (save-buffer)
  (perso/dev/refresh-chromium tab))

(fset 'perso/dev/line-down-section
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("]" 0 "%d")) arg)))

(fset 'perso/dev/line-up-section
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("[" 0 "%d")) arg)))

;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;
