(require 'perso)

(defcustom buffer-manager/keybindings
  ;; Value
  `((", w" . (lambda () (interactive) (subword-mode 'toggle)))
    (?r . (lambda () (interactive) (perso/search-thing-at-point 1)))
    (?s . perso/search-thing-at-point)
    (?l . (lambda () (interactive) (switch-to-buffer (other-buffer))))
    (?/ . hippie-expand)

    (?f . forward-word)
    (?b . backward-word)
    (?p . previous-line)
    (?n . next-line)

    (?d . (lambda () (interactive) (kill-word 1)))
    (?D . (lambda () (interactive) (backward-kill-word 1)))

    (?u . undo-tree-undo)
    (?U . undo-tree-redo)
    (?z . repeat)

    ("M-f" . forward-sexp)
    ("M-b" . backward-sexp)

    ("M-p" . perso/previous-def)
    ("M-n" . perso/next-def)
    
    ("h f" . buffer-manager/helm-find-files)
    ("h b" . buffer-manager/helm-for-files))
  
  "Keybindings for buffer interactions."
  
  ;; Parameters
  :type '(repeat
          (cons :tag "Keybinding"
           (choice
            :tag "Type"
            (integer :tag "Key code, e.g. `?a' or `97'")
            (string :tag "Key combo, e.g. `C-a'"))
           (function :tag "Function")))
  :require 'perso-minor-modes
  :group 'perso
  :group 'window-manager)

(define-minor-mode buffer-manager-mode
    "Buffer interactions helper."
  :global t
  :init-value nil
  :lighter " BM"
  :keymap (perso/build-keymap buffer-manager/keybindings))

(global-set-key (kbd "C-;") '(lambda () (interactive)
                              (buffer-manager-mode 'toggle)))

(provide 'perso-buffer-manager)
