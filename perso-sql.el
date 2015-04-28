(require 'sql)

;;; fix mysql prompt for mariadb
(sql-set-product-feature 'mysql :prompt-regexp "^\\(?:mysql\\|mariadb\\).*> ")

(provide 'perso-sql)
