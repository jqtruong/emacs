(require 'drupal-mode)

(defun drupal/new-module (name)
  (interactive "sName: ")
  (when drupal-rootdir)
  ;; is there a custom module folder? if so cd to it
  ;; (make-directory sites/all/modules . /custom?/ . name) and cd into it
  ;; create files .info and .module (maybe .install)
  )

(defun drupal/behavior-skeleton (name)
  "Add a drupal behavior skeleton to a js file"
  ;; (function($) {
  ;;   Drupal.behaviors.moduleName = {
  ;;     'attach': attach
  ;;   };
  ;;
  ;;   function attach(context) {
  ;;     
  ;;   }
  ;; })(jQuery);
  )

(defun drupal/files/private ()
  (interactive)
  (when drupal-rootdir
    (dired (concat drupal-rootdir "/sites/default/private/"))))

(provide 'perso-drupal)
