;;;;;;;;;;;;;
;; require ;;
;;;;;;;;;;;;;
(require 'em-tramp)
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
  "Function for customized value for eshell-skip-prompt."
  (interactive)
  (search-forward "$ " nil t))

;;;;;;;;;;;;;;
;; settings ;;
;;;;;;;;;;;;;;
(setq shell-switcher-mode t)
;; Crucial to see some colors!
(setq eshell-highlight-prompt nil)
(setq eshell-prompt-function
      (lambda ()
        (concat
         (format-time-string "\n%T " (current-time))
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
         "\n$ ")))

;;;;;;;;;;;
;; hooks ;;
;;;;;;;;;;;
;; If i'm using my IDE making function and eshell is created
;; through it, this will let shell-switcher manage that first shell
;; see https://github.com/DamienCassou/shell-switcher
(add-hook 'eshell-mode-hook 'shell-switcher-manually-register-shell)

;;;;;;;;;;;;;;
;; env vars ;; because eshell doesn't read my .bashrc/.profile
;;;;;;;;;;;;;;
(setenv "JAVA_HOME" "/Library/Java/JavaVirtualMachines/jdk1.7.0_51.jdk/Contents/Home")
(setenv "GRAILS_OPTS" "-Xms2g -Xmx2g -XX:PermSize=128m -XX:MaxPermSize=1024m -XX:+UseConcMarkSweepGC -XX:+CMSClassUnloadingEnabled -server")
(setenv "JAVA_OPTS" "-Djava.awt.headless=true -Xms1G -Xmx1G -XX:MaxPermSize=512m -XX:+UseConcMarkSweepGC")
(setenv "VAGRANTMANAGER_CPU" "3")
(setenv "VAGRANTMANAGER_MEM" "6500")



(provide 'perso-eshell)
