;;;;;;;;;;;;;
;; require ;;
;;;;;;;;;;;;;
(require 'highlight-parentheses)

;;;;;;;;;;;;;;
;; settings ;;
;;;;;;;;;;;;;;
;;; Slime
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/bin/sbcl --dynamic-space-size 12000")
(setq slime-contribs '(slime-fancy slime-cl-indent))

;; (eval-after-load "cl-indent"
;;   '(progn
;;     (put 'cl-flet      'common-lisp-indent-function (get 'flet 'common-lisp-indent-function))
;;     (put 'css-lite:css 'common-lisp-indent-function '(&rest (&whole 2 &rest nil)))
;;     (put 'create-folder-dispatcher-and-handler 'common-lisp-indent-function '(&rest (&whole 2 &rest nil)))
;;     (put 'ps* 'common-lisp-indent-function '(&rest 2))))

(add-hook 'lisp-mode-hook
          '(lambda ()
            (highlight-parentheses-mode)))

;;;;;;;;;
;; end ;;
;;;;;;;;;
(provide 'perso-lisp)
