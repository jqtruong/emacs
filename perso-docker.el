(defun perso/docker-compose-make-buffer-name (action args)
  "Make a buffer name based on ACTION only."
  (format "*docker-compose %s*" action))

(provide 'perso-docker) 
