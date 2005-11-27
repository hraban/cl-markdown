;;; -*- Mode: Lisp; package: CL-USER; Syntax: Common-lisp; Base: 10 -*-

#|

|#

(in-package :common-lisp-user)
(defpackage "ASDF-CL-MARKDOWN" (:use #:cl #:asdf))
(in-package "ASDF-CL-MARKDOWN")

(defsystem cl-markdown 
  :version "0.1"
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :components ((:module "dev"
                        :components ((:file "package")
                                     (:file "definitions"
                                            :depends-on ("package"))
                                     (:file "utilities")
                                     (:file "nanodom")
                                     (:file "inline-patterns")
                                     (:file "pre-processors")
                                     (:file "post-processors")
                                     (:file "auxiliary-classes")
                                     (:file "cl-markdown"
                                            :depends-on ("definitions"))
                                     
                                     (:static-file "notes.text"))))
  
  :depends-on (metatilities cl-ppcre))

