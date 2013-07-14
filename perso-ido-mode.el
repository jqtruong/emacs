;;;;;;;;;;;;;;;
;; functions ;;
;;;;;;;;;;;;;;;
(defadvice ido-find-file (after ido-find-file-ios-counterpart activate)
  ""
  (if (string-match "/iOS/" (buffer-file-name))
      (progn
        (if (string-match "\\.m$" (buffer-name))
            (find-file (replace-regexp-in-string "\\.m$" ".h" (buffer-name)))
          (if (string-match "\\.h$" (buffer-name))
              (find-file (replace-regexp-in-string "\\.h$" ".m" (buffer-name)))))
        (switch-to-buffer nil))))

(defadvice ido-find-file (after ido-rename-zend-buffer-to-classname activate)
  ""
  (when (string-match "/zend/" (buffer-file-name))
    (cond ((string-match "\.php$" (buffer-name)) (zend/rename-buffer-to-classname))
          ((string-match "\.\\(p\\)?html" (buffer-name)) (zend/rename-buffer-to-specify-path-from-project)))))

(defadvice ido-find-file (after ido-rename-org-buffer-to-parent-directory activate)
  ""
  (when (string= ".org" (buffer-name))
    (let ((path (split-string (buffer-file-name) "/")))
      (rename-buffer (format "%s.org" (car (last path 2)))))))

(defun ido-find-file-in-tag-files (&optional next-table)
  (interactive "P")
  (save-excursion
    (let ((enable-recursive-minibuffers t))
      ;; Problem with next-table, or t, is that it doesn't cycle
      ;; through the list of tables, instead just picks the next one.
      ;; So it will only temporarily use the next table in the list
      ;; which is not very useful with more than 2 tables.
      ;; Ideally, i'd like to specify which tags buffer to use, which
      ;; could be implemented instead of using visit-tags-table-buffer.
      (visit-tags-table-buffer (if next-table t)))
    (find-file
     (expand-file-name
      (ido-completing-read
       "Project file: " (tags-table-files) nil t)))))


(provide 'perso-ido-mode)
