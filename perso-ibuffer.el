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
         ("dired" (mode . dired-mode))
         ("Drupal" (filename . "drupal"))
         ("php" (filename . "\\.php$\\|\\.inc$"))
         ("html" (filename . "\\.\\(p\\)?html$\\|\\.tpl$"))
         ("css" (filename . "\\.css"))
         ("js" (filename . "\\.js\\(on\\)?"))
         ("iOS" (filename . "iOS"))
         ("emacs" (or
                   (name . "^\\*scratch\\*$")
                   (name . "^\\*Messages\\*$")))
         ("elisp" (mode . emacs-lisp-mode))
         ("Org" (name . "\\.org$"))
         ("shells" (name . "\\*eshell\\*"))
         ("magit" (name . "^\\*magit\\(: \\|-\\).*\\*$"))
         ("webapp_bloomhealth" (filename . "git/webapp_bloomhealth"))
         ("bloomhealth" (filename . "git/bloomhealth"))
         ("lib_domain" (filename . "git/lib_domain")))))

(add-hook 'ibuffer-mode-hook '(lambda ()
                                (ibuffer-auto-mode 1)
                                (ibuffer-switch-to-saved-filter-groups "default")
                                (when (bound-and-true-p ide-mode-p)
                                  (set-window-parameter (selected-window) 'no-other-window t))))

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
