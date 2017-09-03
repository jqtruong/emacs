;;;;;;;;;;;;;;;;;;
;; screen setup ;;
;;;;;;;;;;;;;;;;;;
(defun perso/screen (&optional off)
  (interactive "P")
  (if off
      (perso/screen-off)
    (perso/screen-on)))

(defun perso/screen-on ()
  (start-process "perso" nil "/home/nymo/scripts/screen"))

(defun perso/screen-off ()
  (start-process "perso" nil "/home/nymo/scripts/screen" "0"))

;;;;;;;;;
;; end ;;
;;;;;;;;;
(provide 'perso-setup)
