;;; to expand literal prog (eg. `<s') shortcuts
(require 'org-tempo)

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

(provide 'perso-org)
