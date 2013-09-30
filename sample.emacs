;; -*- lisp -*-

;;;;;;;;;;;;;;
;; packages ;;
;;;;;;;;;;;;;;
(package-initialize)
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;;;;;;;;;;;
;; perso ;;
;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/perso")
(require 'perso)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs custom variables ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(put 'dired-find-alternate-file 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eshell-prompt-regexp "^[^#$
]* ?[#$] ")
 '(eshell-skip-prompt-function (quote jqt/eshell-skip-prompt))
)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
