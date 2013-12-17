;;;;;;;;;;;;;;
;; settings ;;
;;;;;;;;;;;;;;
(setq perso/lua/function-regexp "function \\(?1:.*\\)([^)])")

;;;;;;;;;;;;;;;
;; functions ;;
;;;;;;;;;;;;;;;
(defun perso/lua/prev-function ()
  )

;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;
(define-key lua-mode-map (kbd "M-p") 'perso/lua/prev-function)
(define-key lua-mode-map (kbd "M-n") 'perso/lua/prev-function)

(provide 'perso-lua)
