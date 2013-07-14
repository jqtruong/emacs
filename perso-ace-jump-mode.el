(require 'ace-jump-mode)

;;;;;;;;;;;
;; setup ;;
;;;;;;;;;;;
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))

;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)



(provide 'perso-ace-jump-mode)
