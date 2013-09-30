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



(provide 'perso-macros)
