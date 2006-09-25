;;; -*- Mode: Lisp; package: CL-USER; Syntax: Common-lisp; Base: 10 -*-

(in-package #:common-lisp-user)
(defpackage #:cl-markdown-test-system (:use #:cl #:asdf))
(in-package #:cl-markdown-test-system)

(defsystem cl-markdown-comparisons
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
                                     )))
  :depends-on (cl-markdown lml2 
			   cl-html-diff html-encode trivial-shell
			   ;; probably not needed if we rearranged more...
			   lift))

