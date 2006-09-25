;;; -*- Mode: Lisp; package: CL-USER; Syntax: Common-lisp; Base: 10 -*-

(in-package #:common-lisp-user)
(defpackage #:cl-markdown-test-system (:use #:cl #:asdf))
(in-package #:cl-markdown-test-system)

(defsystem cl-markdown-test 
  :version "0.1"
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :components ((:module "unit-tests"
                        :components ((:file "package")
				     (:file "test-markdown"
                                            :depends-on ("package"))
                                     (:file "test-chunkers"
                                            :depends-on ("test-markdown"))
                                     (:file "test-regexes"
                                            :depends-on ("test-spans"))
                                     (:file "test-spans"
                                            :depends-on ("test-markdown"))
                                     (:file "test-strippers"
                                            :depends-on ("test-markdown")))))
  :depends-on (cl-markdown lift))


