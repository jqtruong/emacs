(require 'perso-ace-jump-mode)
(require 'perso-emoticons)
(require 'perso-eshell)
(require 'perso-ibuffer)
(require 'perso-ido-mode)
(require 'perso-jabber)
(require 'perso-macros)
(require 'perso-magit)
(require 'perso-misc)
(require 'perso-multiple-cursors)
(require 'perso-paredit)
(require 'perso-web-mode)
(require 'perso-windows)

(require 'hlinum)
(require 'undo-tree)
(require 'winner)
(require 'popwin)

;;;;;;;;;;;;;;
;; hacking! ;;
;;;;;;;;;;;;;;
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

;;;;;;;;;;;;;;
;; settings ;;
;;;;;;;;;;;;;;
(setq-default indent-tabs-mode nil)
(setq ring-bell-function 'jqt/ring-bell)
(global-undo-tree-mode 1)
(ido-mode 1)
(winner-mode 1)
(popwin-mode 1)
(column-number-mode)
(setq lisp-indent-function 'common-lisp-indent-function)
(eval-after-load "cl-indent"
  '(progn
     (put 'cl-flet 'common-lisp-indent-function 
          (get 'flet 'common-lisp-indent-function))))

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

(defun jqt/continue-more (keymaps &optional msg)
  "Helper method for not much at the moment..."
  (if msg
      (message msg))
  (repeater-map-more keymaps msg))

(defun jqt/define-keys (map keymaps)
  "Calls `DEFINE-KEY' on MAP for each pair of command and functions in
KEYMAPS."
  (cl-flet ((string-or-char (key)
              (if (stringp key)
                  (kbd key)
                  (if (characterp key)
                      (vector key)
                      nil))))
    (loop
     for (repeat-key fun) in keymaps
     if (listp fun) do
     (define-key map (string-or-char repeat-key)
       `(lambda () (interactive)
          (apply (car ,fun) (cdr ,fun))
          (setq this-command ,fun)))
     else do
     (define-key map (string-or-char repeat-key)
       `(lambda () (interactive)
                (,fun)
                (setq this-command ',fun))))))

(defun jqt/set-temporary-overlay-map (map-on &optional keep-pred)
  "Like in `subr' but with ability to quit the overlay map with the basic type of the last input event that started it."
  (let* ((map-off (make-sparse-keymap))
         (starter-key (event-basic-type last-input-event))
         (starter-key-sym
          (make-symbol (concat "start-map-with-key-"
                               `(,starter-key))))
         (end-key-sym
          (make-symbol (concat "end-map-with-key-"
                               `(,starter-key))))
         (cursor-color (cdr (assoc 'cursor-color (frame-parameters (selected-frame)))))
         (find-alist-on-sym
          (make-symbol "jqt/set-temporary-overlay-map/find-alist-on"))
         (find-alist-on
          `(lambda (alist) (assoc ',starter-key-sym alist)))
         (find-alist-off-sym
          (make-symbol "jqt/set-temporary-overlay-map/find-alist-off"))
         (find-alist-off
          `(lambda (alist) (assoc ',end-key-sym alist)))
         (remove-alists-sym
          (make-symbol
           "jqt/set-temporary-overlay-map/remove-alists"))
         (remove-alists
          `(lambda ()
             (let ((alist-on (find-if ',find-alist-on-sym
                                      emulation-mode-map-alists))
                   (alist-off (find-if ',find-alist-off-sym
                                       emulation-mode-map-alists)))
               (setq emulation-mode-map-alists
                     (delq alist-off
                           (delq alist-on emulation-mode-map-alists)))
               (set-cursor-color ,cursor-color))))
         (clearfunsym (make-symbol "clear-temporary-overlay-map"))
         (clearfun
          ;; FIXME: Use lexical-binding.
          `(lambda ()
             (unless ,(cond ((null keep-pred) nil)
                            ((eq t keep-pred)
                             (and
                              `(eq this-command
                                   (lookup-key ',map-off
                                               (this-command-keys-vector)))
                              `(eq this-command
                                   (lookup-key ',map-on
                                               (this-command-keys-vector)))))
                            (t `(funcall ',keep-pred)))
               (remove-hook 'pre-command-hook ',clearfunsym)
               (,remove-alists-sym)))))
    ;; The symbol needs to be set in order for the keymap to be
    ;; active.
    (set starter-key-sym t)
    (set end-key-sym t)
    (fset find-alist-on-sym find-alist-on)
    (fset find-alist-off-sym find-alist-off)
    (fset remove-alists-sym remove-alists)
    (fset clearfunsym clearfun)
    (add-hook 'pre-command-hook clearfunsym)
    (set-cursor-color "red")
    (jqt/define-keys map-off `((,starter-key ,remove-alists-sym)))
    (let ((alist-on (list (cons starter-key-sym map-on)))
          (alist-off (list (cons end-key-sym map-off))))
      (push alist-on emulation-mode-map-alists)
      (push alist-off emulation-mode-map-alists))))

(defun jqt/clear-emulation-mode-map-alists ()
  (interactive)
  (setq emulation-mode-map-alists nil))

(defun jqt/clear-face-remapping-alist ()
  (interactive)
  (setq face-remapping-alist nil))

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
