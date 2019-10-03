(provide 'perso-el)

(defun perso/math/interpolate (a₀ a₁ b₀ b₁ steps)
  (interactive "nA₀: \nnA₁: \nnB₀: \nnB₁: \nnSteps: ")
  (let* ((aΔ     (abs (- a₁ a₀)))
         (bΔ     (abs (- b₁ b₀)))
         (ratio  (/ aΔ bΔ))
         (step   (/ bΔ steps))
         (values (loop for i upto bΔ by step
                       collect (+ a₀ (* i ratio)))))
    
    ;; (loop for v in values
    ;;       do (message "%f" v))

    values))

;; (perso/math/interpolate -.5 .5 25.0 37 24)
