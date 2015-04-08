;;;;;;;;;;;;;
;; require ;;
;;;;;;;;;;;;;
(require 'perso-macros)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mostly from http://1lineart.kulaone.com/ ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ascii/flippinmad ()
  "Pattern (there are undisplayed characters):
- LEFT PARENTHESIS #o50
- BOX DRAWINGS LIGHT ARC UP AND LEFT - #o22557
- DEGREE SIGN - #o260
- WHITE SQUARE - #o22641
- DEGREE SIGN - #o260
- FULLWIDTH RIGHT PARENTHESIS - #o177411
- PRESENTATION FORM FOR VERTICAL LEFT PARENTHESIS - #o177065
- SPACE - #o40
- BOX DRAWINGS HEAVY UP AND HORIZONTAL - #o22473
- BOX DRAWINGS HEAVY HORIZONTAL - #o22401
- BOX DRAWINGS HEAVY UP AND HORIZONTAL - #o22473
- RIGHT PARENTHESIS - #o51
"
  (interactive)
  (kill-new "(╯°□°）╯︵ ┻━┻"))

(defun-emoticon ascii/coffeemug "c[_]")
(defun-emoticon ascii/dog "ˁ˚ᴥ˚ˀ")
(defun-emoticon ascii/idklol "¯\\_(ツ)_/¯")
(defun-emoticon ascii/yuno "ლ(ಠ益ಠლ)")
(defun-emoticon ascii/notsureifserious "ﺟ_ﺟ")
(defun-emoticon ascii/idontlike "ಠ_ಠ")
(defun-emoticon ascii/happybirthday "¸¸♬·¯·♩¸¸♪·¯·♫¸¸Happy Birthday To You¸¸♬·¯·♩¸¸♪·¯·♫¸¸ ")
(defun-emoticon ascii/fu "( ಠ_ಠ)凸")
(defun-emoticon ascii/fu2 "凸(｀⌒´メ)凸")
(defun-emoticon ascii/fu3 "")
(defun-emoticon ascii/heart "-`ღ´-")
(defun-emoticon ascii/headphones "d[-_-]b")
(defun-emoticon ascii/jro "JRΩ")



(provide 'perso-emoticons)
