(in-package #:common-lisp-user)

(defpackage #:cl-markdown
  (:use #:common-lisp #:metatilities #:cl-containers #:cl-ppcre)
  (:export
   #:handle-spans
   #:markdown
   #:render-to-stream)
  
  ;; handy (?) regular expressions
  (:export
   #:emphasis-1 #:emphasis-2
   #:strong-1 #:strong-2
   #:backtick
   #:auto-link #:auto-mail
   #:html #:entity
   #:hostname-char #:hostname
   #:pathname-char #:url-pathname
   #:url #:url-no-registers
   #:bracketed #:link+title
   #:reference-link #:inline-link #:link-label))
