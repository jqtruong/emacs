(setq org-timer-default-timer 25)
(add-hook 'org-clock-in-hook (lambda ()
                               (org-timer-set-timer)))

(provide 'perso-org)
