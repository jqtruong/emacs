;;;;;;;;;;;;;
;; require ;;
;;;;;;;;;;;;;
(require 'web-mode)
(require 'hlinum)

;;;;;;;;;;;
;; modes ;;
;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.css$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.gsp$" . web-mode))

;;;;;;;;;;;
;; hooks ;;
;;;;;;;;;;;
(add-hook 'web-mode-hook
          '(lambda ()
             (linum-mode)
             (hlinum-activate)))

;;;;;;;;;;;
;; faces ;;
;;;;;;;;;;;
(set-face-attribute 'web-mode-html-tag-face nil :foreground "#2075c7")
(set-face-attribute 'web-mode-html-attr-name-face nil :foreground "#2075c7")
(set-face-attribute 'web-mode-html-attr-value-face nil :foreground "#259185")



(provide 'perso-web-mode)
