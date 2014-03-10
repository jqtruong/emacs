;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;
(global-set-key (kbd "M-m") (lambda () (interactive)
                               (magit-status default-directory)
                               (delete-other-windows)))

;;;;;;;;;
;; end ;;
;;;;;;;;;
(provide 'perso-magit)
