;;; punctuation.el --- punctuation list -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  Chinese punctuation char table
;;

;;; Code:

(defconst zh-lib--punctuation-alist
  '((?,  . "[，,]")
    (?.  . "[。.]")
    (??  . "[？?]")
    (?:  . "[：:]")
    (?!  . "[！!]")
    (?-  . "[—-]")
    (?~  . "[～~]")
    (?*  . "[×*]")
    (?$  . "[￥$]")
    (?   . "[　 ]")
    (?\; . "[；;]")
    (?\\ . "[、\\]")
    (?\( . "[（(]")
    (?\) . "[）)]")
    (?\< . "[《<]")
    (?\> . "[》>]")
    (?\' . "[‘’「」']")
    (?\" . "[“”『』\"]"))
  "English to Chinese punctuations map.")

;;; punctuation.el ends here
