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
         ("bash"    (mode . sh-mode))
         ("clojure" (mode . clojure-mode))
         ("dired"   (mode . dired-mode))
         ("drupal"  (filename . "drupal"))
         ("elisp"   (mode . emacs-lisp-mode))
         ("emacs"   (or
                     (name . "^\\*scratch\\*$")
                     (name . "^\\*Messages\\*$")))
         ("lisp"    (mode . lisp-mode))
         ("magit"   (or
                     (name . "^\\magit\\(: \\|-\\)")
                     (mode . magit-mode)))
         ("org"     (name . "\\.org\\(\\.gpg\\)?$"))
         ("php"     (filename . "\\.php$\\|\\.inc$"))
         ("python"  (mode . python-mode))
         ("shells"  (name . "\\*eshell\\*"))
         ("web"     (mode . web-mode)))))

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
