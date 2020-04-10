(require 'rjsx-mode)

;; (add-to-list 'auto-mode-alist '("\\(react-app\\|jsx\\|client\\)/.*\\.js[x]?\\'" . rjsx-mode))
(add-hook 'rjsx-mode-hook '(lambda () (subword-mode)))

(provide 'perso-rjsx-mode)
