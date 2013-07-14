(require 'perso-ace-jump-mode)
(require 'perso-emoticons)
(require 'perso-eshell)
(require 'perso-ibuffer)
(require 'perso-ido-mode)
(require 'perso-jabber)
(require 'perso-macros)
(require 'perso-misc)
(require 'perso-multiple-cursors)
(require 'perso-paredit)
(require 'perso-web-mode)
(require 'perso-windows)

(require 'hlinum)
(require 'undo-tree)

;;;;;;;;;;;;;;
;; hacking! ;;
;;;;;;;;;;;;;;
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

;;;;;;;;;;;;;;
;; settings ;;
;;;;;;;;;;;;;;
(setq ring-bell-function 'jqt/ring-bell)
(setq global-undo-tree-mode 1)
(ido-mode 1)

;;;;;;;;;;;;;;;
;; functions ;;
;;;;;;;;;;;;;;;
;; Copied from somewhere on the web.
(defun yank-pop-forwards (arg)
  (interactive "p")
  (yank-pop (- arg)))

(defun jqt/ring-bell ()
  "Do nothing."
  )

(defun jqt/copy-buffer-name ()
  "Puts selected buffer's name in the kill ring."
  (interactive)
  (kill-new (buffer-name)))

(defun jqt/continue (fun)
  "Helper method to set the repeat-key before calling `repeater-map'."
  (let ((repeat-key (event-basic-type last-input-event)))
    (repeater-map repeat-key fun)))

(defun jqt/continue-more (keymaps)
  "Helper method for not much at the moment..."
  (repeater-map-more keymaps))

;;;;;;;;;;;
;; modes ;;
;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;
;; overrides
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-e") 'end-of-visual-line)
;; custom
(global-set-key (kbd "C-x F")   'ido-find-file-in-tag-files)
(global-set-key (kbd "C-c r")   'replace-string)
(global-set-key (kbd "C-c u")   'uncomment-region)
(global-set-key (kbd "C-! b")   'jqt/copy-buffer-name)
(global-set-key (kbd "C-! t")   'jqt/insert-current-date-time)
(global-set-key (kbd "C-! s")   'jqt/insert-seconds-from-date)
(global-set-key (kbd "C-@")     'browse-url)
(global-set-key (kbd "M-? t")   'jqt/convert-from-unix-timestamp)
(global-set-key (kbd "M-? p")   'jqt/point)
(global-set-key (kbd "M-.")     'etags-select-find-tag)
(global-set-key (kbd "M-Y")     'yank-pop-forwards)
;; map
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'comment-box)



(provide 'perso)
