;;;;;;;;;;;;;
;; require ;;
;;;;;;;;;;;;;
(require 'web-mode)
(require 'hlinum)
(require 'hl-line)

;;;;;;;;;;;
;; modes ;;
;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.css$"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.gsp$"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . web-mode))

;;;;;;;;;;;
;; hooks ;;
;;;;;;;;;;;
(add-hook 'web-mode-hook
          '(lambda ()
            (linum-mode)
            (hlinum-activate)
            ;; possibly fixed after web-mode update
            ;; (hl-line-mode -1)
            ))

;;;;;;;;;;;
;; faces ;;
;;;;;;;;;;;
;; (set-face-attribute 'web-mode-html-tag-face nil :foreground
;; "#2075c7")
(set-face-attribute 'web-mode-html-tag-face nil :foreground "#aa5588")
(set-face-attribute 'web-mode-html-attr-name-face nil :foreground "#5588aa")
(set-face-attribute 'web-mode-html-attr-value-face nil :foreground "#88aa55")

;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;
(define-key web-mode-map (kbd "C-;") nil) ; C-; personally reserved for window management.

;;;;;;;;;
;; end ;;
;;;;;;;;;
(provide 'perso-web-mode)
