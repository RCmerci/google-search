;;; test-google-search.el --- tests                  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  rcmerci

;; Author: rcmerci <rcmerci@rcmercis-rmbp.local>

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

(require 'ert)
(require 'google-search)


(ert-deftest test//google-search//update-search-history//base ()
  (let ((old-history google-search//search-history)
	(old-max     google-search//max-search-history)
	(request-1 "test request 1")
	(request-2 "test request 2")
	(request-3 "test request 3"))
    (setq google-search//max-search-history (+ 3 (length google-search//search-history)))
    (unwind-protect
	(progn
	  (google-search//update-search-history request-1)
	  (should (equal (cons request-1 old-history) google-search//search-history))
	  (google-search//update-search-history request-2)
	  (should (equal (cadr google-search//search-history) request-1))
	  (should (equal (car google-search//search-history)  request-2))
	  (google-search//update-search-history request-3)
	  (should (equal (cadr google-search//search-history) request-2))
	  (should (equal (car google-search//search-history)  request-3))
	  )
      
      (setq google-search//search-history old-history)
      (setq google-search//max-search-history old-max))))


(ert-deftest test//google-search//update-search-history//max-search-history ()
  (let ((old-history google-search//search-history))
    (unwind-protect
	(progn
	  (dotimes (n (- google-search//max-search-history (length google-search//search-history)))
	    (setq google-search//search-history (cons (format "test%S" n) google-search//search-history)))
	  (should (equal google-search//max-search-history (length google-search//search-history))) 
	  (google-search//update-search-history "test-max")
	  (should (equal google-search//max-search-history (length google-search//search-history)))
	  (should (equal "test-max" (car google-search//search-history)))
	  )
      (setq google-search//search-history old-history))))


(ert-deftest test//google-search//generate-request-string ()
  (let ((request-1 " \t\n")
	(request-2 "233")
	(request-3 " 2 3 3 ")
	(request-4 " 2 3 3")
	(request-5 "测 试 中 文"))
    (should-error (google-search//generate-request-string request-1) :type 'user-error)
    (should (equal '("233" . "233") (google-search//generate-request-string request-2)))
    (should (equal '("2 3 3" . "2+3+3") (google-search//generate-request-string request-3)))
    (should (equal '("2 3 3" . "2+3+3") (google-search//generate-request-string request-4)))
    (should (equal `("测 试 中 文" . ,(url-encode-url "测+试+中+文")) (google-search//generate-request-string request-5)))))




(provide 'test-google-search)
;;; test-google-search.el ends here


