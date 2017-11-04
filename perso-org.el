(setq org-timer-default-timer 25)
(add-hook 'org-clock-in-hook (lambda ()
                               (org-timer-set-timer)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (emacs-lisp . t)))

(add-hook 'org-mode-hook
          (lambda () (set-fill-column 60)))

(provide 'perso-org)
