;;; perso-web-mode --- Personal Web Mode Configurations

;;; Commentary:

;;;;;;;;;;;;;
;; require ;;
;;;;;;;;;;;;;
(require 'web-mode)
(require 'hlinum)
(require 'hl-line)
(require 'flycheck)

;;; Code:

;;;;;;;;;;;
;; modes ;;
;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.htm$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.gsp$"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

;;;;;;;;;;;;;;
;; settings ;;
;;;;;;;;;;;;;;
(add-to-list 'web-mode-comment-formats '("javascript" . "// "))

;;; JSX syntax highlighting
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

;; Disable the default jslint:
(setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers
                                                 '(javascript-jshint json-jsonlist)))

;; Enable eslint checker for web-mode
(flycheck-add-mode 'javascript-eslint 'web-mode)
;; Enable flycheck globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;;;;;;;;;;;
;; hooks ;;
;;;;;;;;;;;
(add-hook 'web-mode-hook
          '(lambda ()
            (linum-mode)
            (hlinum-activate)
            (add-node-modules-path)
            (prettier-js-mode)
            (subword-mode)))

;;;;;;;;;;;
;; faces ;;
;;;;;;;;;;;
(set-face-attribute 'web-mode-html-tag-face nil :foreground "#aa5588")
(set-face-attribute 'web-mode-html-attr-name-face nil :foreground "#5588aa")
(set-face-attribute 'web-mode-html-attr-value-face nil :foreground "#88aa55")

;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;
(define-key web-mode-map (kbd "M-p") 'perso/web/previous-block)
(define-key web-mode-map (kbd "M-n") 'perso/web/next-block)

;;;;;;;;;
;; end ;;
;;;;;;;;;
(provide 'perso-web-mode)

;;; perso-web-mode ends here
