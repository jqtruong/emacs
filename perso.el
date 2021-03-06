;;; perso --- Personal Helpers

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;
;; packages ;;
;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Enable defer and ensure by default for use-package
;; Keep auto-save/backup files separate from source code:  https://github.com/scalameta/metals/issues/1027
(setq use-package-always-defer t
      use-package-always-ensure t
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(defvar my-packages '(ace-jump-buffer
                      ace-jump-mode
                      ag
                      auto-dim-other-buffers
                      base16-theme
                      dic-lookup-w3m
                      expand-region
                      gitignore-mode
                      helm
                      helm-ls-git
                      highlight-parentheses
                      hlinum
                      inf-mongo
                      jabber
                      jade-mode
                      js-comint
                      json-mode
                      json-reformat
                      json-snatcher
                      jsx-mode
                      less-css-mode
                      logito
                      multiple-cursors
                      neotree
                      dash
                      org
                      pkg-info
                      elisp-slime-nav
                      magit
                      idle-highlight-mode
                      paredit
                      rjsx-mode
                      undo-tree
                      w3m
                      web-mode
                      names
                      shell-switcher
                      smex))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;;;;;;;;;
;; Lisp ;;
;;;;;;;;;;
;; (eval-after-load 'cl-indent
;;   `(progn
;;      (put 'css-lite:css 'common-lisp-indent-function '(&rest (&whole 2 &rest nil)))))

;;;;;;;;;;;;;
;; require ;;
;;;;;;;;;;;;;
(require 'perso-ace-jump-mode)
(require 'perso-bash)
(require 'perso-dev)
(require 'perso-docker)
(require 'perso-emoticons)
(require 'perso-eshell)
(require 'perso-go)
(require 'perso-ibuffer)
(require 'perso-ido-mode)
(require 'perso-jabber)
(require 'perso-js)
;; (require 'perso-lisp)
(require 'perso-macros)
(require 'perso-magit)
(require 'perso-misc)
(require 'perso-multiple-cursors)
(require 'perso-node)
(require 'perso-org)
(require 'perso-paredit)
(require 'perso-rjsx-mode)
(require 'perso-setup)
(require 'perso-sql)
(require 'perso-web-mode)
(require 'perso-windows)
(require 'ag)
(require 'auto-dim-other-buffers)
(require 'bs)
(require 'expand-region)
(require 'helm)
(require 'linum)
(require 'highlight-parentheses)
(require 'hlinum)
(require 'idle-highlight-mode)
(require 'undo-tree)
(require 'winner)

;;;;;;;;;;;;;;
;; settings ;;
;;;;;;;;;;;;;;
(setq-default indent-tabs-mode nil)
(setq ring-bell-function 'jqt/ring-bell)
(global-undo-tree-mode 1)
(ido-mode 1)
(winner-mode 1)
(column-number-mode)
(auto-dim-other-buffers-mode 1)
(menu-bar-mode 0)
(tool-bar-mode 0)

;;;;;;;;;;;
;; hooks ;;
;;;;;;;;;;;
;; instead, i prefer linum-mode hlinum combo
(add-hook 'prog-mode-hook '(lambda ()
                            (linum-mode)
                            (hlinum-activate)))

;; TODO move to perso-lisp or something
(add-hook 'lisp-mode-hook '(lambda ()
                            (idle-highlight-mode)))
(add-hook 'emacs-lisp-mode-hook '(lambda ()
                                  (highlight-parentheses-mode 1)
                                  (idle-highlight-mode)))
(add-hook 'clojure-mode-hook '(lambda ()
                               (highlight-parentheses-mode 1)))
(add-hook 'sql-mode-hook
          (lambda ()
            (sql-highlight-mysql-keywords)))

;;; TODO move to perso-c

(font-lock-add-keywords 'c-mode
                        '(("\\(\\w+\\)\\s-*\("
                           (1 font-lock-function-name-face)))
                        t)

;;;;;;;;;;;;;
;; advices ;;
;;;;;;;;;;;;;
(defadvice find-file (after rename-org-buffer-to-parent-directory activate)
  ""
  (when (string= ".org" (buffer-name))
    (let ((path (split-string (buffer-file-name) "/")))
      (rename-buffer (format "%s.org" (car (last path 2)))))))

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

(defun jqt/copy-buffer-name (&optional n)
  "Puts selected buffer's name, N, in the kill ring."
  (interactive "P")
  (kill-new (if n (perso/path-minus-n n) (buffer-name))))

(defun perso/path-minus-n (n)
  "Return current path of current file name minus `N' parts from the head."
  (let* ((path-parts (split-string (buffer-file-name) "/"))
         (len (length path-parts)))
    (if (< len n)
        (message "%s contains %d parts, less than given %d." path-parts len n)
        (mapconcat
         ;; tried 'string or 'symbol-name but they both err on empty string.
         (lambda (s) s)
         ;; remove n + 1 parts to include first element, the empty
         ;; string, since all paths start with `/'.
         (dotimes (i (1+ n) path-parts)
           (setq path-parts (cdr path-parts)))
         "/"))))

(defun jqt/continue (fun)
  "Helper method FUN to set the repeat-key before calling `repeater-map'."
  (let ((repeat-key (event-basic-type last-input-event)))
    (repeater-map repeat-key fun)))

(defun jqt/continue-more (keymaps &optional msg)
  "Helper method for not much at the moment..."
  (if msg
      (message msg))
  (repeater-map-more keymaps msg))

(defun jqt/next-buffer-window-condition (buffer window func &optional switch-p)
  (if window
       (if (eq window (selected-window))
           (progn
             (bury-buffer buffer)
             (set-window-buffer window
                                (if (listp func)
                                    (apply (car func) (cdr func))
                                  (funcall func))))
         (select-window window))
     (if switch-p
         (switch-to-buffer buffer)
       (pop-to-buffer buffer))))

(defun jqt/next-buffer-with-regexp (regexp &optional i)
  "Filters buffer list for names that start with `REGEXP'."
  (elt
   (remove-if-not
    (lambda (buffer) (string-match regexp (buffer-name buffer)))
    (buffer-list))
   (or i 0)))

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
          (intern (concat "start-map-with-key-"
                               (number-to-string starter-key))))
         (end-key-sym
          (intern (concat "end-map-with-key-"
                               (number-to-string starter-key))))
         (cursor-color (cdr (assoc 'cursor-color (frame-parameters (selected-frame)))))
         (find-alist-on-sym
          (intern "jqt/set-temporary-overlay-map/find-alist-on"))
         (find-alist-on
          `(lambda (alist) (assoc ',starter-key-sym alist)))
         (find-alist-off-sym
          (intern "jqt/set-temporary-overlay-map/find-alist-off"))
         (find-alist-off
          `(lambda (alist) (assoc ',end-key-sym alist)))
         (remove-alists-sym
          (intern
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
    ;; TODO customize that color
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

(defun perso/search-thing-at-point (&optional backward-p)
  (interactive)
  (let ((symbol (thing-at-point 'symbol)))
    (if backward-p
        (search-backward symbol nil t)
        (search-forward symbol nil t))))

(defcustom perso/regexp/def "(\\(def[^ ]+\\) \\(?1:[^ ]+\\)"
  "Elisp `def'initions.")

(defun perso/previous-def ()
  (interactive)
  (re-search-backward perso/regexp/def nil t)
  (goto-char (match-beginning 1)))

(defun perso/next-def ()
  (interactive)
  (re-search-forward perso/regexp/def nil t)
  (goto-char (match-beginning 1)))

(defun perso/define-key-helper/string-or-char (key)
  (if (stringp key)
      (kbd key)
      (if (characterp key)
          (vector key)
          nil)))

(defun perso/build-keymap (keybindings)
  "i was hoping there would be a way to dynamically build the keymap 
but `define-minor-mode' is macro that statically sets the map or at
least just once... until i know more about how that macro works and
wrap it or create my own since it does do something like 
`set-temporary-overlay-map'

the keybindings are already customizable but the minor-modes
will have to be re- evaluated to re- build the keymap and so is 
customizingly useless"
  (interactive)
  (let ((map (make-sparse-keymap)))
    (loop for keybinding in keybindings
       for (key . function) = keybinding
       for key = (perso/define-key-helper/string-or-char key)
       do (define-key map key `,function))
    map))

;;; something's off, it worked the first time but then stopped after
;; making the keybinding. it's possible that i should map it to C-x
;; C-S-F but not sure why
;; (fset 'perso/goto-from-*grep*
;;    (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([19 58 13 67108896 134217830 134217847 1 24 6 return 134217831 134217831 25 return] 0 "%d")) arg)))

(defun perso/docker/start ()
  (interactive)
  (async-shell-command "sudo systemctl start docker"))

(defun perso/docker/status ()
  (interactive)
  (async-shell-command "systemctl status docker"))

;;;;;;;;;;;
;; modes ;;
;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;
;; overrides
(global-set-key (kbd "C-s")       'isearch-forward-regexp)
(global-set-key (kbd "C-r")       'isearch-backward-regexp)
(global-set-key (kbd "C-S-s")     'ag)
(global-set-key (kbd "C-e")       'end-of-visual-line)
(global-set-key (kbd "C-x C-f")   'ido-find-file)
(global-set-key (kbd "C-x b")     'ido-switch-buffer)
(global-set-key (kbd "C-x B")     'bs-show)
(global-set-key (kbd "C-x C-b")   'helm-for-files)
(global-set-key (kbd "C-x C-o")   (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "M-'")       'other-frame)
(global-set-key (kbd "C-x O")     'other-frame)
(global-set-key (kbd "C-x C-S-o") (lambda () (interactive) (other-frame -1)))
(global-set-key (kbd "C-x 2")     (lambda () (interactive)
                                     (split-window-vertically)
                                     (other-window 1)))
(global-set-key (kbd "C-x 3")     (lambda () (interactive)
                                     (split-window-horizontally)
                                     (other-window 1)))
(global-set-key (kbd "M-x")       'smex)

;;; i should use the other function that switches the keybinding for a
;;; named function to call another, so it's more obvious what changed
;;; with these overrides.

;; custom
(global-unset-key (kbd "C-c r"))
(global-set-key (kbd "C-c r r")       'replace-string)
(global-set-key (kbd "C-c r c")       'comment-region)
(global-set-key (kbd "C-c r u")       'uncomment-region)
(global-set-key (kbd "C-c r e")       'er/expand-region)
(global-set-key (kbd "C-! b")         'jqt/copy-buffer-name)
(global-set-key (kbd "s-<return> ! t")         'jqt/insert-current-date-time)
(global-set-key (kbd "s-<return> ! s")         'jqt/insert-seconds-from-date)
(global-set-key (kbd "C-@")           'browse-url)
(global-set-key (kbd "s-<return> t")  'jqt/convert-from-unix-timestamp)
(global-set-key (kbd "s-<return> p")  'jqt/point)
(global-set-key (kbd "M-Y")           'yank-pop-forwards)
(global-set-key (kbd "C-z")           'repeat)
(global-set-key (kbd "C-c w u")       'winner-undo)
(global-set-key (kbd "C-c w r")       'winner-redo)
(global-set-key (kbd "C-c l")         'org-store-link)
(global-set-key (kbd "C-S-s")         'ag)
(global-set-key (kbd "M-\"")          (lambda () (interactive) (other-frame -1)))
(global-set-key (kbd "s-\"")          'previous-window-any-frame)
(global-set-key (kbd "M-W")           (lambda () (interactive)
                                        (let ((symbol (thing-at-point 'symbol)))
                                          (message "`%s' → kill ring" symbol)
                                          (kill-new symbol))))
;; map
(define-key emacs-lisp-mode-map (kbd "C-c C-c")         'comment-box)
(define-key emacs-lisp-mode-map (kbd "M-p")             'perso/previous-def)
(define-key emacs-lisp-mode-map (kbd "M-n")             'perso/next-def)
(define-key emacs-lisp-mode-map (kbd "C-x C-S-e")       'eval-print-last-sexp)
(define-key lisp-interaction-mode-map (kbd "C-x C-S-e") 'eval-print-last-sexp)
(define-key paredit-mode-map (kbd "C-\"")               'paredit-meta-doublequote)
(define-key paredit-mode-map (kbd "M-\"")               (lambda () (interactive) (other-frame -1)))
(define-key ag-mode-map (kbd "k")                       '(lambda () (interactive)
                                                           (let (kill-buffer-query-functions) (kill-buffer))
                                                           (delete-window)))

;;;;;;;;;
;; end ;;
;;;;;;;;;
(provide 'perso)
