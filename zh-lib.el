;;; zh-lib.el --- Zhongwen library for Emacs -*- lexical-binding: t; -*-

;;; Copyright (C) 2020-2021 dalu

;; Author: dalu <mou.tong@qq.com>
;; URL: https://github.com/dalugm/zh-lib.el
;; Version: 0.1
;; Package-Requires: (emacs "24")
;; Keywords: Chinese, library

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;  Zhongwen library for Emacs.
;;

;;; Code:

(defgroup zh-lib nil
  "Zhongwen library for Emacs."
  :group 'convenience)

(defcustom zh-lib-with-punctuation t
  "Whether search include Chinese punctuation."
  :type 'boolean
  :group 'zh-lib)

(defcustom zh-lib-scheme 'simplified-quanpin-all
  "Zhongwen scheme.

Possible values:
- nil: user specified char table.
- 'simplified-quanpin-all: quanpin for all simplified characters.
- 'simplified-quanpin-common: quanpin of common used 3500 simplified characters.
- 'simplified-xiaohe-all: xiaohe shuangpin for all simplified characters.
- 'simplified-pinyinjiajia-all: pinyinjiajia shuangpin for all simplified characters.
- 'simplified-ziranma-all: ziranma shuangpin for all simplified characters.
- 'simplified-weiruan-all: weiruan shuangpin for all simplified characters.
- 'traditional-quanpin-all: quanpin for all traditional characters.
- 'simplified-traditional-quanpin-all: quanpin for all simplified and traditional characters."
  :type 'symbol
  :group 'zh-lib)

(defvar zh-lib-char-table nil
  "User specified char table.")

;; dynamically loaded scheme
(defvar zh-lib--simplified-quanpin-all)
(defvar zh-lib--simplified-quanpin-common)
(defvar zh-lib--simplified-xiaohe-all)
(defvar zh-lib--traditional-quanpin-all)
(defvar zh-lib--simplified-traditional-quanpin-all)
(defvar zh-lib--punctuation-alist)

(defconst zh-lib--file load-file-name
  "Zhongwen library file name.")

(defun zh-lib--load-char-table-file (file)
  "Load char table FILE."
  (load (expand-file-name file (file-name-directory zh-lib--file))))

(defun zh-lib--get-char-table ()
  "Get char table for Chinese."
  (cond
    ;; use simplified quanpin
    ((eq zh-lib-scheme 'simplified-quanpin-all)
      (unless (boundp 'zh-lib--simplified-quanpin-all)
        (zh-lib--load-char-table-file "simplified-quanpin-all"))
      zh-lib--simplified-quanpin-all)
    ;; use simplified common
    ((eq zh-lib-scheme 'simplified-quanpin-common)
      (unless (boundp 'zh-lib--simplified-quanpin-common)
        (zh-lib--load-char-table-file "simplified-quanpin-common"))
      zh-lib--simplified-quanpin-common)
    ;; use simplified xiaohe
    ((memq zh-lib-scheme (list
                           'simplified-xiaohe-all
                           'simplified-pinyinjiajia-all
                           'simplified-ziranma-all
                           'simplified-weiruan-all))
      (unless (boundp 'zh-lib--simplified-xiaohe-all)
        (zh-lib--load-char-table-file "simplified-xiaohe-all"))
      zh-lib--simplified-xiaohe-all)
    ;; use traditional quanpin
    ((eq zh-lib-scheme 'traditional-quanpin-all)
      (unless (boundp 'zh-lib--traditional-quanpin-all)
        (zh-lib--load-char-table-file "traditional-quanpin-all"))
      zh-lib--traditional-quanpin-all)
    ;; use simplified and traditional quanpin
    ((eq zh-lib-scheme 'simplified-traditional-quanpin-all)
      (unless (boundp 'zh-lib--simplified-traditional-quanpin-all)
        (zh-lib--load-char-table-file "simplified-traditional-quanpin-all"))
      zh-lib--simplified-traditional-quanpin-all)
    ;; user specified char table
    ((not zh-lib-scheme)
      zh-lib-char-table)))

(defun zh-lib--get-punctuation-alist()
  "Get punctuation alist."
  (unless (boundp 'zh-lib--punctuation-alist)
    (zh-lib--load-char-table-file "punctuation"))
  zh-lib--punctuation-alist)

(defun zh-lib-build-regexp-char
  (char &optional no-punctuation-p only-chinese-p)
  "Build regexp for a character CHAR.

NO-PUNCTUATION-P: Punctuations are not included.
ONLY-CHINESE-P: English characters are not included."
  (let ((diff (- char ?a))
        regexp)
    (if (or (>= diff 26) (< diff 0))
        (or (and (not no-punctuation-p)
                 (assoc-default
                   char
                   (zh-lib--get-punctuation-alist)))
            (regexp-quote (string char)))
      (setq regexp (nth diff (zh-lib--get-char-table)))
      (if only-chinese-p
          (if (string= regexp "")
              regexp
            (format "[%s]" regexp))
        (format "[%c%s]" char regexp)))))

(defun zh-lib-build-regexp-string
  (str &optional no-punc-p only-chinese-p)
  "Build regexp for a string STR.

NO-PUNC-P: Punctuations are not included.
ONLY-CHINESE-P: English characters are not included."
  (mapconcat
    (lambda (char)
      (zh-lib-build-regexp-char char no-punc-p only-chinese-p))
    str
    ""))

(defun zh-lib-build-regexp (thing)
  "Build regexp form THING for search."
  (cond
    ((integerp thing)
      (zh-lib-build-regexp-char
        thing (not zh-lib-with-punctuation)))
    ((stringp thing)
      (zh-lib-build-regexp-string
        thing (not zh-lib-with-punctuation)))))

(provide 'zh-lib)

;;; zh-lib.el ends here
