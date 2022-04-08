;;; punctuation.el --- punctuation list -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  Chinese punctuation char table
;;

;;; Code:

(defconst zh-lib--punctuation-alist
  '((?~  . "[～~]")
    (?!  . "[！!]")
    (?@  . "[©®℗@]")
    (?#  . "[№#]")
    (?$  . "[￥€£¢₩$]")
    (?%  . "[％°℃‰‱℉%]")
    (?^  . "[…^]")
    (?*  . "[×＊·・*]")
    (?-  . "[—-]")
    (?=  . "[々〃=]")
    (?:  . "[：:]")
    (?,  . "[，,]")
    (?.  . "[。.]")
    (?/  . "[、､／÷/]")
    (??  . "[？?]")
    (?   . "[　 ]")
    (?{  . "[『〖｛{]")
    (?}  . "[』〗｝}]")
    (?<  . "[《«〈‹<]")
    (?>  . "[》»〉›>]")
    (?\[ . "[\[「【［]")
    (?\] . "[\]」】］]")
    (?\; . "[；;]")
    (?\\ . "[、\\]")
    (?\( . "[（(]")
    (?\) . "[）)]")
    (?\' . "[‘’']")
    (?\" . "[“”\"]"))
  "English to Chinese punctuation's map.")

;;; punctuation.el ends here
