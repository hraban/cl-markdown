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
  :components ((:module "test"
                        :components ((:file "package")
                                     (:file "utilities"
                                            :depends-on ("package"))
                                     (:file "test-markdown"
                                            :depends-on ("utilities")))))
                                     
  
  :depends-on (cl-markdown lml2 cl-html-parse cl-fad))

