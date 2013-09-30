;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;
(global-set-key (kbd "M-m") (lambda () (interactive)
			      (magit-status default-directory)))


(provide 'perso-magit)
