;;;;;;;;;;;;;
;; require ;;
;;;;;;;;;;;;;
(require 'lua-mode)

;;;;;;;;;;;;;;
;; settings ;;
;;;;;;;;;;;;;;
(setq perso/lua/regexp/function "function \\(?1:.*\\)([^)])")

;;;;;;;;;;;;;;;
;; functions ;;
;;;;;;;;;;;;;;;
(defun perso/lua/previous-function ()
  ;; TODO macro these previous/next helpers
  (interactive)
  (re-search-backward perso/lua/regexp/function nil t)
  (goto-char (match-beginning 1)))

(defun perso/lua/next-function ()
  (interactive)
  (re-search-forward perso/lua/regexp/function nil t)
  (goto-char (match-beginning 1)))

;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;
(define-key lua-mode-map (kbd "M-p") 'perso/lua/previous-function)
(define-key lua-mode-map (kbd "M-n") 'perso/lua/next-function)

(provide 'perso-lua)
