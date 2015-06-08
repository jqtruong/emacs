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
currently within with either the problems dataset or the sample
ones."
  (interactive "P")
  (let* ((path (file-name-directory buffer-file-name))
         (script (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
         (dataset (concat(if test-p (concat path "samples/") "~/Downloads/rosalind_") script ".txt"))
         (func (perso/bash/prev-func/name)))
    (async-shell-command (concat ". " buffer-file-name " && " func " " dataset)))))

(defun perso/bash/run/rosalind/test ()
  "See `perso/bash/run/rosalind'."
  (interactive)
  (perso/bash/run/rosalind t))

(defun perso/bash/prev-func/name ()
  "Puts the previous function name into the kill ring."
  (interactive)
  (save-excursion
    (perso/bash/prev-func)
    (let ((func (thing-at-point 'symbol)))
      func)))

(defun jqt/test ()
  (interactive)
  (let ((func (perso/bash/prev-func/name)))
    (message "%s" func)))

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
            (set-fill-column 120)))

;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;
(define-key sh-mode-map (kbd "s-c s-c") 'perso/bash/run/rosalind)
(define-key sh-mode-map (kbd "M-p")     'perso/bash/prev-func)
(define-key sh-mode-map (kbd "M-n")     'perso/bash/next-func)


(provide 'perso-bash)
