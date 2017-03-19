;;;;;;;;;;;;;
;; require ;;
;;;;;;;;;;;;;
(require 'highlight-parentheses)
(require 'js)

;;;;;;;;;;;
;; hooks ;;
;;;;;;;;;;;
(add-hook 'js-mode-hook '(lambda ()
                          (highlight-parentheses-mode 1)))
(add-hook 'json-mode 'flymake-json-load)

;;;;;;;;;;;;;;
;; settings ;;
;;;;;;;;;;;;;;
(modify-syntax-entry ?` "\"" js-mode-syntax-table)
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cycle through methods ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom perso/regexp/js/function
  "function\\( *\\|[^(]*\\)([^)]*)\\|\\bangular\\."
  "Javascript functions and Angular method calls.")

(defcustom perso/regexp/js/angular-constructs
  "\\.\\(module\\|controller\\|config\\|run\\|directive\\|factory\\)"
  "Angular constructors.")

(defun perso/js/previous-function ()
  (interactive)
  (re-search-backward perso/regexp/js/function nil t)
  (goto-char (match-beginning 1)))

(defun perso/js/next-function ()
  (interactive)
  (re-search-forward perso/regexp/js/function nil t)
  (goto-char (match-beginning 1)))

(defun perso/js/angular/previous-construct ()
  (interactive)
  (re-search-backward perso/regexp/js/angular-constructs nil t)
  (goto-char (match-beginning 1)))

(defun perso/js/angular/next-construct ()
  (interactive)
  (re-search-forward perso/regexp/js/angular-constructs nil t)
  (goto-char (match-beginning 1)))

;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;
(define-key js-mode-map (kbd "M-p")     'perso/js/previous-function)
(define-key js-mode-map (kbd "M-n")     'perso/js/next-function)
(define-key js-mode-map (kbd "C-M-p")   'perso/js/angular/previous-construct)
(define-key js-mode-map (kbd "C-M-n")   'perso/js/angular/next-construct)
(define-key js-mode-map (kbd "C-c C-w") 'subword-mode)


(provide 'perso-js)
