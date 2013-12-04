(require 'perso)

;; wouldn't it be better to create minor modes with activation keyboard
;; shortcut à la `subword-mode'?

(defvar jqt/tom/cursor-color nil)
(defcustom jqt/tom/cursor-color "red"
  ;; TODO choose color à la face 
  :type  'string
  :group 'jqt)

(defun jqt/tom/on (map-on &optional keep-pred)
  "Like in `subr' but with ability to quit the overlay map with the basic type of the last input event that started it."
  (let* ((starter-key (event-basic-type last-input-event))
         (key-code (number-to-string starter-key))
         (starter-key-sym
          (intern (concat "jqt/tom/on-code-" key-code)))
         ;; `clearfun' would be here because keep-pred might be
         ;; different for each overlay map.
         (clearfunsym (make-symbol "clear-temporary-overlay-map"))
         (clearfun
          ;; FIXME: Use lexical-binding.
          `(lambda ()
             (unless ,(cond ((null keep-pred) nil)
                            ((eq t keep-pred)
                             (and
                              `(eq this-command
                                   (lookup-key 'jqt/tom/map-off
                                               (this-command-keys-vector)))
                              `(eq this-command
                                   (lookup-key ',map-on
                                               (this-command-keys-vector)))))
                            (t `(funcall ',keep-pred)))
               (remove-hook 'pre-command-hook ',clearfunsym)
               (jqt/tom/destroy ,key-code)
               (,remove-alists-sym)))))
    ;; The symbol needs to be set in order for the keymap to be
    ;; active.
    (set starter-key-sym t)
    (set end-key-sym t)
    (fset clearfunsym clearfun)
    (add-hook 'pre-command-hook clearfunsym)
    (set-cursor-color jqt/tom/cursor-color)
    (jqt/tom/setup-rest key-code)
    (jqt/define-keys jqt/tom/map-off `((,starter-key jqt/tom/destroy)))
    (let ((alist-on (list (cons starter-key-sym map-on))))
      (push alist-on emulation-mode-map-alists))))

(defun jqt/tom/setup-rest (key-code)
  (setq jqt/tom/cursor-color
        (cdr (assoc 'cursor-color (frame-parameters (selected-frame)))))
  (let* ((map-off (make-sparse-keymap))
         (end-key-sym (intern (concat "jqt/tom/off-code-" key-code))))
         (alist-off (list (cons end-key-sym map-off)))
         (push alist-off emulation-mode-map-alists))))

(defun jqt/tom/destroy (key-code)
  (let ((alist-on (find-if ',find-alist-on-sym
                           emulation-mode-map-alists))
        (alist-off (find-if ',find-alist-off-sym
                            emulation-mode-map-alists))
        (starter-key-str (number-to-string starter-key)))
    (loop
       for alist in emulation-mode-map-alists
         sym in syms
       while (jqt/tom/sym-not-in-alist-p sym alist))
    (setq emulation-mode-map-alists
          (delq alist-off
                (delq alist-on emulation-mode-map-alists)))
    (set-cursor-color ,cursor-color)))

(defun jqt/tom/sym-not-in-alist-p (sym alist))
