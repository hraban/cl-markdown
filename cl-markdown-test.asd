;; cl-markdown -- Markdown parser in CL
;; Copyright (C) <year>  <name of author>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, version 3 of the License.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(in-package #:common-lisp-user)
(defpackage #:cl-markdown-test-system (:use #:cl #:asdf))
(in-package #:cl-markdown-test-system)

(defsystem cl-markdown-test 
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Hraban Luyat <hraban@0brg.net>"
  :licence "GPL-3.0-only"
  :components ((:module
		"setup"
		:pathname "unit-tests/"
		:components 
		((:file "package")
		 (:file "utilities"
			:depends-on ("package"))
		 (:file "test-markdown"
			:depends-on ("package"))))			     
	       (:module 
		"unit-tests"
		:depends-on ("setup")
		:components ((:file "test-chunkers")
			     (:file "test-snippets")
			     (:file "test-links")
			     (:file "test-brackets-and-includes")
			     (:file "brackets-with-empty-lines")
			     (:file "test-headers")
			     (:file "test-dl")
			     (:file "test-anchors"))))
  :depends-on (:cl-markdown
	       :lift
	       :trivial-shell))


