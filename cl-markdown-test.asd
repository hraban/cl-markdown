;;; -*- Mode: Lisp; package: CL-USER; Syntax: Common-lisp; Base: 10 -*-

#|

|#

(in-package :common-lisp-user)
(defpackage "CL-MARKDOWN-TEST-SYSTEM" (:use #:cl #:asdf))
(in-package "CL-MARKDOWN-TEST-SYSTEM")

(defsystem cl-markdown-test 
  :version "0.1"
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :components ((:module "unit-tests"
                        :components ((:file "package")
                                     (:file "framework" 
                                            :depends-on ("package"))
                                     (:file "comparison" 
                                            :depends-on ("framework"))
                                     (:file "test-markdown"
                                            :depends-on ("package"))
                                     (:file "test-chunkers"
                                            :depends-on ("test-markdown"))
                                     (:file "test-regexes"
                                            :depends-on ("test-spans"))
                                     (:file "test-spans"
                                            :depends-on ("test-markdown")))))
                                     
  
  :depends-on (cl-markdown lift cl-html-diff html-encode metashell))

