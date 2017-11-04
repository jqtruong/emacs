;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;
(global-set-key (kbd "M-m") (lambda () (interactive)
                               (magit-status-internal default-directory)
                               (delete-other-windows)))

;;;;;;;;;
;; end ;;
;;;;;;;;;
(provide 'perso-magit)
