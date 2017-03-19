;;;;;;;;;;;;;
;; require ;;
;;;;;;;;;;;;;
(require 'highlight-parentheses)

;;;;;;;;;;;;;;
;; settings ;;
;;;;;;;;;;;;;;
(setq lisp-indent-function 'common-lisp-indent-function)
(eval-after-load "cl-indent"
  '(progn
    (put 'cl-flet      'common-lisp-indent-function (get 'flet 'common-lisp-indent-function))
    (put 'css-lite:css 'common-lisp-indent-function '(&rest (&whole 2 &rest nil)))
    (put 'create-folder-dispatcher-and-handler 'common-lisp-indent-function '(&rest (&whole 2 &rest nil)))
    (put 'ps* 'common-lisp-indent-function '(&rest 2))))

(add-hook 'lisp-mode-hook
          '(lambda ()
            (highlight-parentheses-mode)))

;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;
(fset 'buffer/sbcl
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([24 53 98 115 98 99 108 return] 0 "%d")) arg)))

;;;;;;;;;
;; end ;;
;;;;;;;;;
(provide 'perso-lisp)
