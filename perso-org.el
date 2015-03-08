(setq org-timer-default-timer 25)
(add-hook 'org-clock-in-hook (lambda ()
                               (org-timer-set-timer)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh . t)))

(provide 'perso-org)
