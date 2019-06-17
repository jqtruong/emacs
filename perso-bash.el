;;;;;;;;;;;;;
;; require ;;
;;;;;;;;;;;;;
(require 'sh-script)

;;;;;;;;;;;;;;;;;;;;;;
;; custom variables ;;
;;;;;;;;;;;;;;;;;;;;;;
(defcustom perso/regexp/bash/function
  "^\\(function *\\)?\\(?1:[^ ]+\\) *\\(()\\) *{"
  "Bash functions.")

;;;;;;;;;;;;;;;
;; functions ;;
;;;;;;;;;;;;;;;
(defun perso/bash/run/rosalind (&optional test-p)
  "On a Rosalind script, run the function of which the cursor is
currently within, with either the problems dataset or the sample
ones."
  (interactive "P")
  (let* ((path (file-name-directory buffer-file-name)) ;get file directory path
         (script (file-name-nondirectory buffer-file-name))
         (id (file-name-sans-extension script))
         (name (file-name-extension script))
         (dataset (concat (if test-p (concat path "samples/") "~/Downloads/rosalind_") name ".txt"))
         (func (perso/bash/prev-func/name)))
    (async-shell-command (concat ". " buffer-file-name " && " func " " dataset))))

(defun perso/bash/run/rosalind/test ()
  "See `perso/bash/run/rosalind'."
  (interactive)
  (perso/bash/run/rosalind t))

(defun perso/bash/prev-func/name ()
  "Returns the previous function name, nearest to cursor, including from within the body of one."
  (interactive)
  (save-excursion
    (perso/bash/prev-func)
    (thing-at-point 'symbol)))

(defun perso/bash/prev-func ()
  "Search backwards for a function definition."
  (interactive)
  (re-search-backward perso/regexp/bash/function nil t)
  (goto-char (match-beginning 1)))

(defun perso/bash/next-func ()
  "Search backwards for a function definition."
  (interactive)
  (re-search-forward perso/regexp/bash/function nil t)
  (goto-char (match-end 1)))


;;;;;;;;;;;
;; setup ;;
;;;;;;;;;;;
(add-hook 'sh-mode-hook
          '(lambda ()
            (set-fill-column 80)))


;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;
(define-key sh-mode-map (kbd "s-c s-c") 'perso/bash/run/rosalind)
(define-key sh-mode-map (kbd "M-p")     'perso/bash/prev-func)
(define-key sh-mode-map (kbd "M-n")     'perso/bash/next-func)


(provide 'perso-bash)
