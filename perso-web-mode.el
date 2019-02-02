;;;;;;;;;;;;;
;; require ;;
;;;;;;;;;;;;;
(require 'web-mode)
(require 'hlinum)
(require 'hl-line)

;;;;;;;;;;;
;; modes ;;
;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.htm$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.gsp$"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

;;;;;;;;;;;;;;
;; settings ;;
;;;;;;;;;;;;;;
(add-to-list 'web-mode-comment-formats '("javascript" . "// "))

;;; @TODO: move this out, but also update as necessary. it currently matches
;;; parentheses by functions, but maybe one day i'd like to cycle solely through
;;; functions or classes.
(defcustom perso/regexp/code-block
  "class [A-Z][^{]+{\\|\\(function\\)?\\( *\\|[^(]*\\)([^)]*)\\|\\bangular\\."
  "Javascript functions and Angular method calls.")

(perso/macro/previous-block perso/web/previous-block perso/regexp/code-block)
(perso/macro/next-block perso/web/next-block perso/regexp/code-block)

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
(set-face-attribute 'web-mode-html-tag-face nil :foreground "#aa5588")
(set-face-attribute 'web-mode-html-attr-name-face nil :foreground "#5588aa")
(set-face-attribute 'web-mode-html-attr-value-face nil :foreground "#88aa55")


;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;
(define-key web-mode-map (kbd "M-p")     'perso/web/previous-block)
(define-key web-mode-map (kbd "M-n")     'perso/web/next-block)

;;;;;;;;;
;; end ;;
;;;;;;;;;
(provide 'perso-web-mode)
