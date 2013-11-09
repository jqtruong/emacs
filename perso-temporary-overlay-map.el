(require 'perso)

(defvar jqt/tom/cursor-color nil)
(defvar jqt/tom/map-off nil)

(defun jqt/tom/on (map-on &optional keep-pred)
  "Like in `subr' but with ability to quit the overlay map with the basic type of the last input event that started it."
  (let* ((map-off (make-sparse-keymap))
         (starter-key (event-basic-type last-input-event))
         (starter-key-sym
          (intern (concat "start-map-with-code-"
                          (number-to-string starter-key))))
         (end-key-sym
          (intern (concat "end-map-with-code-"
                          (number-to-string starter-key))))
         (cursor-color (cdr (assoc 'cursor-color (frame-parameters (selected-frame)))))
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

(defun jqt/tom/setup ()
  (setq jqt/tom/cursor-color (cdr (assoc 'cursor-color (frame-parameters (selected-frame)))))
  (setq jqt/tom/map-off (make-sparse-keymap)))

(defun jqt/tom/destroy ()
  (let ((alist-on (find-if ',find-alist-on-sym
                           emulation-mode-map-alists))
        (alist-off (find-if ',find-alist-off-sym
                            emulation-mode-map-alists)))
    (setq emulation-mode-map-alists
          (delq alist-off
                (delq alist-on emulation-mode-map-alists)))
    (set-cursor-color ,cursor-color)))
