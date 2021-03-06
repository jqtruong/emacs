(defmacro repeater-map (repeat-key fun)
  "For one-key maps to repeatly call `fun'."
  `(set-temporary-overlay-map
    (let ((map (make-sparse-keymap)))
      (define-key map (vector ,repeat-key)
        ,fun)
      map) t))

(defmacro repeater-map-more (keymaps &optional msg)
  "For complex maps in which multiple repeatable keys are attached to
functions."
  `(set-temporary-overlay-map
    (let ((map (make-sparse-keymap)))
      (loop
       for (repeat-key fun) in ,keymaps
       do (define-key map (vector repeat-key)
            `(lambda () (interactive)
               (if ,,msg
		   (message ,,msg))
               (if (listp ',fun)
                   (apply (car ,fun) (cdr ,fun))
		 (,fun)))))
      map) t))

(defmacro defun-emoticon (name ascii)
  ""
  `(defun ,name ()
     ""
     (interactive)
     (kill-new ,ascii)))

(defmacro defun-copy-string (fun-name string)
  "Creates a function with `fun-name' that puts the `string' in the kill-ring."
  `(defun ,fun-name ()
     ""
     (interactive)
     (kill-new ,string)))

(defmacro perso/macro/previous-block (fun-name regexp)
  `(defun ,fun-name ()
     (interactive)
     (re-search-backward ,regexp nil t)
     (goto-char (match-beginning 1))))

(defmacro perso/macro/next-block (fun-name regexp)
  `(defun ,fun-name ()
     (interactive)
     (re-search-forward ,regexp nil t)
     (goto-char (match-beginning 1))))

;;;;;;;;;;;;;;
;; the end. ;;
;;;;;;;;;;;;;;

(provide 'perso-macros)
