;;; perso-node.el --- NodeJS and related tools stuff  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Jerome Truong

;; Author: Jerome Truong <nymo@tack>
;; Keywords: node, npm

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defun perso/node/start (buffer-name)
  "Start node in its own window each time, instead of new/separate *Async Shell Command* buffers"
  (interactive "sBuffer Name: ")

  (when (or (null buffer-name)
            (zerop (length (jqt/trim-string buffer-name))))
    (setq buffer-name "node"))  
  
  (let* ((root (perso/node/find-root))
         (buffer-name (concat (file-name-nondirectory (directory-file-name (file-truename root))) "/" buffer-name)))
    (async-shell-command (concat "cd " root "; npm start") buffer-name)))

(defun perso/node/run (&optional script)
  "Runs `npm run SCRIPT' from the project root."
  (interactive "sScript: ")
  (let ((script (if (zerop (length (jqt/trim-string script))) "build" script)))
    (async-shell-command (concat "cd " (perso/node/find-root) "; npm run " script))))

(defmacro perso/node/run/script (script)
  "Creates a function which calls `(perso/node/run SCRIPT)'."
  `(defun ,(intern (concat "perso/node/run/" script)) ()
     "Runs `npm run SCRIPT' from the project root."
     (interactive)
     (async-shell-command (concat "cd " (perso/node/find-root) "; npm run " ,script))))

(perso/node/run/script "build")
(perso/node/run/script "build:less")

(defun perso/node/find-root (&optional dir)
  "Finds root package.json by traversing upwards from current
  directory."
  (interactive)
  (let* ((cwd (or dir default-directory))
         (files (directory-files cwd)))
    (if (member "package.json" files)
        cwd
        (perso/node/find-root(concat cwd "../")))))

(provide 'perso-node)
;;; perso-node.el ends here
