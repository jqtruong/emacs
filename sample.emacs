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
 '(custom-enabled-themes (quote (base16-default)))
 '(custom-safe-themes (quote ("1affe85e8ae2667fb571fc8331e1e12840746dae5c46112d5abb0c3a973f5f5a" default)))
 '(eshell-prompt-regexp "^[^#$
]* ?[#$] ")
 '(eshell-skip-prompt-function (quote jqt/eshell-skip-prompt))
 '(jabber-alert-presence-hooks nil)
 '(jabber-chat-header-line-format (quote ("" (:eval (jabber-jid-displayname jabber-chatting-with)) " " (:eval (let ((buddy (jabber-jid-symbol jabber-chatting-with))) (propertize (or (cdr (assoc (get buddy (quote show)) jabber-presence-strings)) (get buddy (quote show))) (quote face) (or (cdr (assoc (get buddy (quote show)) jabber-presence-faces)) (quote jabber-roster-user-online))))) "      " (:eval (jabber-fix-status (get (jabber-jid-symbol jabber-chatting-with) (quote status)))) "	" jabber-events-message "	" jabber-chatstates-message)))
 '(js-indent-level 2)
 '(magit-repo-dirs (quote ("~/git/")))
 '(org-babel-load-languages (quote ((lisp . t) (emacs-lisp . t))))
 '(safe-local-variable-values (quote ((Base . 10) (Package . HUNCHENTOOT) (Syntax . COMMON-LISP) (whitespace-line-column . 80) (lexical-binding . t))))
 '(web-mode-css-indent-offset 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "#55ff55"))))
 '(hl-line ((t (:inherit highlight :background "#303030"))))
 '(linum-highlight-face ((t (:inherit default :background "#55ff55" :foreground "black"))))
 '(mode-line ((t (:background "#505050" :foreground "#e0e0e0" :box (:line-width -1 :style released-button) :height 0.9))))
 '(region ((t (:background "RoyalBlue4"))))))
(put 'erase-buffer 'disabled nil)
 
