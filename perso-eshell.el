;;;;;;;;;;;;;
;; require ;;
;;;;;;;;;;;;;
(require 'em-tramp)
(require 'esh-module)
(require 'shell-switcher)

;;;;;;;;;;;;;;;
;; functions ;;
;;;;;;;;;;;;;;;
(defun pwd-repl-home (pwd)
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
         (home-len (length home)))
    (if (and
         (>= (length pwd) home-len)
         (equal home (substring pwd 0 home-len)))
        (concat "~" (substring pwd home-len))
      pwd)))

(defun curr-dir-git-branch-string (pwd)
  "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
  (interactive)
  (when (and (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let ((git-output (shell-command-to-string (concat "git branch | grep '\\*' | sed -e 's/^\\* //'"))))
      (if (> (length git-output) 0)
          (propertize (concat " * "
                              (substring git-output 0 -1))
                      'face `((:foreground "green4")
                              bold))
        (propertize " (no branch)" 'face `(:foreground "#dc322f"))))))

(defun curr-dir-svn-string (pwd)
  (interactive)
  (when (and (eshell-search-path "svn")
             (locate-dominating-file pwd ".svn"))
    (concat "[s:"
            (cond ((string-match-p "/trunk\\(/.*\\)?" pwd)
                   "trunk")
                  ((string-match "/branches/\\([^/]+\\)\\(/.*\\)?" pwd)
                   (match-string 1 pwd))
                  (t
                   "(no branch)"))
            "] ")))

(defun grails-eshell-magic ()
  "Handle grails output gracefully.
Added to eshell-output-filter-functions through customization."
  (when (eshell-interactive-process))
  (let (position (mark (current-buffer)))
    (save-excursion
      (goto-char eshell-last-output-block-begin)
      (beginning-of-line)
      (while (re-search-forward "\033\\[1A\033\\[[0-9]+D\033\\[0K" position t)
        (replace-match "" t t)
        (previous-line)
        (delete-region
         (line-beginning-position)
         (progn (forward-line 1) (point)))))))

(defun jqt/eshell-skip-prompt ()
  "Function for customized value for eshell-skip-prompt.")

(defun perso/eshell/use-existing-window (orig-fun &rest args)
  "If a window exists with an eshell buffer, switch to it first."
  (interactive "P")
  (let ((eshell-buffers (remove-if-not
                         (lambda (buffer) (string-match "\*eshell\*" (buffer-name buffer)))
                         (buffer-list))))
    (if (loop
           for buffer in eshell-buffers
           for window = (get-buffer-window buffer)
           when window
           return (select-window window))
        (sswitcher--prepare-for-fast-key)
      (progn
        (apply orig-fun nil)
        (forward-page)))))

(advice-add 'shell-switcher-switch-buffer :around
            #'perso/eshell/use-existing-window)

;;;;;;;;;;;;;;
;; settings ;;
;;;;;;;;;;;;;;
(setq eshell-skip-prompt-function 'jqt/eshell-skip-prompt)
(setq shell-switcher-mode t)
(setq eshell-cmpl-ignore-case t)

;;; sudo https://emacs.stackexchange.com/questions/5608/how-to-let-eshell-remember-sudo-password-for-two-minutes/5619
(add-to-list 'eshell-modules-list 'eshell-tramp)
(setq eshell-prefer-lisp-functions t)   ; mainly for sudo atm 2018-08-23 12:15:08
(setq password-cache t)                 ; enable password caching
(setq password-cache-expiry 60)         ; time in seconds

;;; Crucial to see some colors!
(setq eshell-highlight-prompt nil)
(setq eshell-prompt-function
      (lambda ()
        (let* ((bat (if (fboundp 'battery-linux-sysfs) (battery-linux-sysfs) '((112 . "always 100"))))
               (status (cdr (assoc 66 bat)))
               (eta (cdr (assoc 116 bat)))) ; mostly showing 10 hours remaining which is completely false, right?
            (concat
             (format-time-string "\n%T " (current-time))
             "["
             (cdr (assoc 112 bat)) "%"
             (cond ((string= status "Discharging") (concat " -" eta))
                   ((string= status "Charging") (concat " +" eta))
                   (t ""))
             "] "
             (propertize (user-login-name) 'face `(:foreground "#2b5dcd"))
             ":"
             ((lambda (p-lst)
                (if (> (length p-lst) 3)
                    (concat
                     (mapconcat (lambda (elm) (if (zerop (length elm)) ""
                                                (substring elm 0 1)))
                                (butlast p-lst 3)
                                "/")
                     "/"
                     (mapconcat (lambda (elm) elm)
                                (last p-lst 3)
                                "/"))
                  (mapconcat (lambda (elm) elm)
                             p-lst
                             "/")))
              (split-string (pwd-repl-home (eshell/pwd)) "/"))
             (or (curr-dir-git-branch-string (eshell/pwd))
                 (curr-dir-svn-string (eshell/pwd)))
             "\n$ "))))

;;;;;;;;;;;
;; hooks ;;
;;;;;;;;;;;
;; If i'm using my IDE making function and eshell is created
;; through it, this will let shell-switcher manage that first shell
;; see https://github.com/DamienCassou/shell-switcher
(add-hook 'eshell-mode-hook 'shell-switcher-manually-register-shell)

;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;



(provide 'perso-eshell)
