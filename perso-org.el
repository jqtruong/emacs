;;; package --- Org Mode

;;; Commentary:

;;; Code:

(require 'package)

(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (emacs-lisp . t)))

(add-hook 'org-mode-hook
          (lambda ()
            (set-fill-column 60)
            (auto-fill-mode 1)))

(defun perso/org/insert-time ()
    "Add the time."
  (interactive)
  (let* ((seconds (floor (time-to-seconds (current-time))))
         (time-string (format-time-string "%H:%M" (seconds-to-time seconds))))
    (insert time-string)
    (org-indent-line)
    (insert "\n-----")
    (org-indent-line)
    (insert "\n")
    (org-indent-line)))

;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;
(define-key org-mode-map "\C-c\M-." 'perso/org/insert-time)
(define-key org-mode-map (kbd "C-! <return>") 'org-time-stamp-inactive)

(provide 'perso-org)
;;; perso-org.el ends here
