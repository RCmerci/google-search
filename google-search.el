;;; google-search.el --- search the internet by google, bing  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  rcmerci

;; Author: rcmerci <rcmerci@rcmercis-rmbp.local>
;; Keywords: hypermedia
;; Package-Requires: ((emacs "24.4") (swiper "20160124.429"))
;; Version: 20160314.1

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



;;; Code:
(defvar google-search//search-engines
  '(("bing" . "http://global.bing.com/search?q=%s")
    ("google" . "https://www.google.com/?gws_rd=ssl#q=%s"))
  "搜索引擎。")

(defvar google-search//max-search-history 100
  "最大搜索历史数量.")

(defvar google-search//search-history nil
  "搜索历史。 
 格式：
 (\"content\" ... ) ")

(defvar google-search//search-history-file "~/.emacs.d/search-google-history")


;;;###autoload
(defun google-search (request search-engine)
  "search the internet."
  (interactive
   (list
    (if (use-region-p)
	(buffer-substring-no-properties (region-beginning) (region-end))
      (progn
       ;; read history file if not yet
       (if (null google-search//search-history)
	   (google-search//read-recent-search-history-file))
       (ivy-read "search: " google-search//search-history :sort nil)))
    (if current-prefix-arg
	(ivy-read "search engine: " google-search//search-engines :require-match t)
      (caar google-search//search-engines))))
  
  (let ((strs (google-search//generate-request-string request)))
    (google-search//update-search-history (car strs))
    (browse-url (format (cdr (assoc search-engine google-search//search-engines)) (cdr strs)))

    ;; if region is active, deactive it.
    ;; see also `region-active-p`, here should use `use-region-p`.
    (if (use-region-p)
	(deactivate-mark))))

(defun google-search//generate-request-string (request)
  "translate request string into two string,
one as the parameter of `browse-url`,
another one is stored in `google-search//search-history`."
  (let* ((store-str
	  (replace-regexp-in-string "\\(^[[:space:]]*\\)\\|\\([[:space:]]*$\\)" ""
				    (replace-regexp-in-string "\n" " " request)))
	 (search-str
	  (url-encode-url (replace-regexp-in-string "[[:space:]]+" "+" store-str))))
    (when (cl-equalp "" store-str)
      (user-error "empty content for searching."))
    `(,store-str . ,search-str)))


(defun google-search//update-search-history (request)
  (setq google-search//search-history (cl-delete request google-search//search-history :test 'equal))
  (setq google-search//search-history (cons request google-search//search-history))
  (if (> (length google-search//search-history) google-search//max-search-history)
      (setq google-search//search-history (cl-subseq google-search//search-history 0 -1))))


(defun google-search//save-recent-search-history ()
  "保存 `google-search//search-history` 到 `google-search//search-history-file`. "
  ;; make sure it exists.
  (unless  (file-exists-p google-search//search-history-file)
      (write-region "" nil google-search//search-history-file))
  
  (if (and (file-readable-p google-search//search-history-file)
	   (file-writable-p google-search//search-history-file))
      (with-temp-buffer
	(insert (format "%S" google-search//search-history))
	(write-file google-search//search-history-file))
    (error "file: \"%s\", may not readable|writable." google-search//search-history-file)))

(defun google-search//read-recent-search-history-file ()
  "设置 `google-search//search-history` 为 `google-search//search-history-file`. "
  (if (and (file-exists-p google-search//search-history-file)
	   (file-readable-p google-search//search-history-file))
      (with-temp-buffer
	(insert-file-contents google-search//search-history-file)
	(setq google-search//search-history (read (current-buffer))))))

(defconst hook-function-alist
  '(
    (kill-emacs-hook . google-search//save-recent-search-history)
    )
  "google-search 用到的 hook 及对应 function"
  )

(dolist (hook-function hook-function-alist)
  (add-hook (car hook-function) (cdr hook-function)))

(provide 'google-search)
;;; google-search.el ends here
