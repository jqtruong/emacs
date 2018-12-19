;;;;;;;;;;;;;
;; require ;;
;;;;;;;;;;;;;
(require 'ibuffer)

;;;;;;;;;;;;;;;
;; functions ;;
;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; settings ;;
;;;;;;;;;;;;;;
(setq ibuffer-saved-filter-groups
      '(("default"
         ("dired"  (mode . dired-mode))
         ("Drupal" (filename . "drupal"))
         ("php"    (filename . "\\.php$\\|\\.inc$"))
         ("html"   (filename . "\\.\\(p\\)?html$\\|\\.tpl$"))
         ("css"    (filename . "\\.css"))
         ("js"     (filename . "\\.js\\(on\\)?"))
         ("emacs"  (or
                    (name . "^\\*scratch\\*$")
                    (name . "^\\*Messages\\*$")))
         ("elisp"  (mode . emacs-lisp-mode))
         ("lisp"   (mode . lisp-mode))
         ("bash"   (mode . sh-mode))
         ("Org"    (name . "\\.org$"))
         ("shells" (name . "\\*eshell\\*"))
         ("magit"  (name . "^\\*magit\\(: \\|-\\).*\\*$")))))

(add-hook 'ibuffer-mode-hook '(lambda ()
                               (ibuffer-auto-mode 1)
                               (ibuffer-switch-to-saved-filter-groups "default")))

(setq ibuffer-formats
      '((mark modified read-only " "
              (name 18 18 :left elide)
              (size 9 -1 :right) " "
              (mode 16 16 :left :elide) " "
              filename-and-process)
        (mark " " (name 16 -1) " " filename)
        (mark modified read-only " "
              (name 18 18 :left elide)
              filename)
        (mark modified read-only " " name)))

(setq ibuffer-show-empty-filter-groups nil)

;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-x C-b") 'ibuffer)



(provide 'perso-ibuffer)
