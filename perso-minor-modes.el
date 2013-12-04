(require 'perso)

;;; TODO macrofy these two custom vars.
(defcustom window-manager/keybindings
  ;; Value
  '((?f . (lambda () (interactive) (other-window 1)))
    (?b . (lambda () (interactive) (other-window -1))))
  
  "Keybindings for window interactions."
  
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

(define-minor-mode window-manager-mode
    "Window interactions helper."
  :global t
  :init-value nil
  :lighter " WM"
  :keymap (perso/build-keymap window-manager/keybindings))

(provide 'perso-minor-modes)
