;;; -*- Mode: Lisp; package: CL-USER; Syntax: Common-lisp; Base: 10 -*-

(in-package #:common-lisp-user)
(defpackage #:cl-markdown-test-system (:use #:cl #:asdf))
(in-package #:cl-markdown-test-system)

(defsystem cl-markdown-test 
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
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
			     #+(or)
			     (:file "test-regexes"
				    :depends-on ("test-spans"))
			     #+(or)
			     (:file "test-spans"
				    :depends-on ("test-markdown"))
			     #+(or)
			     (:file "test-strippers"
				    :depends-on ("test-markdown")))))
  :depends-on (:cl-markdown
	       :lift
	       :trivial-shell))


