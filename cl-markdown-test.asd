(in-package #:common-lisp-user)
(defpackage #:cl-markdown-test-system (:use #:cl #:asdf))
(in-package #:cl-markdown-test-system)

(defsystem cl-markdown-test 
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :components ((:module 
		"unit-tests"
		:components ((:file "package")
			     (:file "test-markdown"
				    :depends-on ("package"))
			     (:file "test-chunkers"
				    :depends-on ("test-markdown"))
			     (:file "test-snippets"
				    :depends-on ("test-markdown"))
			     (:file "test-links"
				    :depends-on ("test-snippets"))
			     #+(or)
			     (:file "test-regexes"
				    :depends-on ("test-spans"))
			     #+(or)
			     (:file "test-spans"
				    :depends-on ("test-markdown"))
			     #+(or)
			     (:file "test-strippers"
				    :depends-on ("test-markdown")))))
  :depends-on (:cl-markdown :lift :trivial-shell))


