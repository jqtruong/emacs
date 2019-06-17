(require 'thingatpt)

;; list of functions that needs some work but are usable fttb.

;;;;;;;;;;;;;;;
;; functions ;;
;;;;;;;;;;;;;;;
(defun jqt/convert-from-unix-timestamp (seconds &optional no-message-p)
  ""
  (interactive (list (if current-prefix-arg
                         (string-to-number (read-from-minibuffer "Seconds: "))
                       (number-at-point))))
  (let ((date-string (format-time-string "%Y-%m-%d %T" (seconds-to-time seconds))))
    (if no-message-p
        ;; used non-interactively
        date-string
      (message "%s" date-string))))

(defun jqt/date-string-to-seconds (string)
  ""
  (interactive "sDate string: ")
  (insert (format "%d" (float-time (date-to-time string)))))

(defun jqt/convert-newlines-to (start end separator)
  ""
  (interactive "r\nsSeparator: ")
  ;; Move to prompt.
  (search-forward-regexp "^\$ $" nil t)
  (end-of-line)
  (insert (format "%s" (jqt/string-friendly-rectangle-lines start end separator))))

(defun jqt/strings-in-rectangle (start end separator)
  ""
  (interactive "r\nsSeparator: ")
  (kill-new (jqt/string-friendly-rectangle-lines start end separator)))

(defun jqt/string-friendly-rectangle-lines (start end &optional separator)
  "Lines within a rectangle defined by region but where each line is
delimited by its ending string rather than the rectangle's regular
boundaries.

Optional SEPARATOR to concatenate the collected lines and return a string."
  (interactive "r")
  (let* (;; Each line in the region.
         (lines (split-string (buffer-substring-no-properties start end) "\n" t))
         ;; The difference in length between the first two lines
         ;; (chars-to-ltrim (- (length (cadr lines)) (length (car lines))))
         ;; Number of characters to trim off the left is
         (chars-to-ltrim (- ;; the difference between
                          start ;; point at the beginning of the region
                          (save-excursion ;; and point at the beginning of the first line.
                            (goto-char start) ;; Because currently at the end of the region.
                            (beginning-of-line)
                            (point))))
         ;; Left trim each line a predefined # of chars, but skip the first line.
         (ltrimmed-lines (append (list (car lines))
                                 (mapcar #'(lambda (line)
                                            (substring line chars-to-ltrim))
                                         (cdr lines))))
         ;; Substring of each string from beginning to end of line or next break.
         (strings (mapcar #'(lambda (line)
                             (jqt/string-until-next-break line))
                          ltrimmed-lines)))
    (if separator
        (mapconcat 'concat strings separator)
      strings)))

(defun jqt/string-until-next-break (string)
  ""
  (with-temp-buffer
    (insert (replace-regexp-in-string "^ *" "" string))
    (goto-char 1)
    (let ((start 1)
          (end (if (search-forward " " nil t)
                   (1- (point))
                 (progn
                   (end-of-line)
                   (point)))))
      (buffer-substring start end))))

(defun jqt/point ()
  ""
  (interactive)
  (message "%d" (point)))

(defun jqt/insert-current-date-time (&optional in-seconds-p)
  ""
  (interactive "P")
  (let ((seconds (floor (time-to-seconds (current-time)))))
    (if in-seconds-p
        (insert (format "%s" seconds))
      (insert (jqt/convert-from-unix-timestamp seconds t)))))

(defun jqt/insert-seconds-from-date (date)
  ""
  (interactive "sDate: ")
  (insert (format-seconds "%s" (time-to-seconds (date-to-time date)))))

(defun jqt/trim-string (string)
  ""
  (replace-regexp-in-string "^[ \n\t]*" ""
                            (replace-regexp-in-string "[ \n\t]*$" "" string)))

(defun jqt/insert-seconds-from-date (date)
  ""
  (interactive "sDate: ")
  (insert (format-seconds "%s" (time-to-seconds (date-to-time date)))))

;;;;;;;;;;;
;; MySQL ;;
;;;;;;;;;;;

;; Turn off line-wrap.
(add-hook 'sql-interactive-mode-hook '(lambda () (toggle-truncate-lines 1)))

(defun mysql/table-name-at-point ()
  ""
  (let (;; Store current point.
        (start (point))
        ;; Store point at the end of the table name.
        (end (1- (search-forward-regexp "[\s;]"))))
    ;; Store table name kill ring for later use.
    (copy-region-as-kill start end)
    ;; Set name to table name in buffer.
    (buffer-substring start end)))

(defun mysql/table-name (&optional option)
  "Option:
nil - at point
1 - partial
2 - full"
  (case option
    (1 (mysql/table-name-from-partial))
    (2 ())
    (t (thing-at-point 'symbol))))

(defun mysql/desc-table (&optional option)
  ""
  (interactive "P")
  (push-mark)
  (let ((name (mysql/table-name option)))
    ;; Move to mysql prompt.
    (search-forward-regexp "> $" nil t)
    ;; Enter command.
    (insert (format "desc %s;" name))
    (comint-send-input)))

(defun mysql/select-fields-in-rectangle (start end)
  ""
  (interactive "r")
  (let ((fields (jqt/string-friendly-rectangle-lines start end ", ")))
    ;; Move to mysql prompt.
    (search-forward-regexp "> $" nil t)
    (insert (format "select %s from " fields))))

(defun perso/copy-symbol-at-point ()
  (interactive)
  (kill-new (thing-at-point 'symbol)))

(defun my-ip-filter (p ip)
  (let ((ip (replace-regexp-in-string "\n" "" ip)))
    (message "Got ip: %s" ip)
    (kill-new ip)))

(defun perso/ip ()
  (interactive)
  (let ((proc (start-process "my-ip" nil "/home/nymo/bash/my-ip")))
    (set-process-filter proc #'my-ip-filter)))

;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-S-w")  'perso/copy-symbol-at-point)



(provide 'perso-misc)
