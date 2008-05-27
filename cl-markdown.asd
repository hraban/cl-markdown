(in-package #:common-lisp-user)

(defpackage #:cl-markdown-system (:use #:cl #:asdf))
(in-package #:cl-markdown-system)

(defsystem cl-markdown 
  :version "0.10.0"
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :components
  ((:static-file "COPYING")
   (:module "dev"
	    :components
	    ((:file "package")
	     (:file "api"
		    :depends-on ("package"))
	     (:file "definitions"
		    :depends-on ("package"))
	     (:file "macros"
		    :depends-on ("package"))
	     (:file "class-defs"
		    :depends-on ("definitions"))
	     (:file "utilities"
		    :depends-on ("macros" "package" "definitions" "class-defs"))
	     (:file "spans"
		    :depends-on ("regexes" "class-defs"))
	     (:file "regexes"
		    :depends-on ("package"))
	     (:file "markdown"
		    :depends-on ("utilities" "class-defs" 
					     "spans" "definitions"))
	     (:file "html"
		    :depends-on ("utilities" "class-defs" "spans"))
	     (:file "plain"
		    :depends-on ("utilities" "class-defs" "spans"))
	     (:file "multiple-documents"
		    :depends-on ("definitions"))
	     (:file "epilogue"
		    :depends-on ("markdown"))
	     (:static-file "notes.text")))
               
   (:module "extensions"
	    :pathname #.(make-pathname :directory '(:relative "dev"))
	    :components
	    ((:file "extension-mechanisms")
	     (:file "extensions" :depends-on ("extension-mechanisms"))
	     (:file "footnotes" :depends-on ("extension-mechanisms")))
	    :depends-on ("dev"))
               
   (:module "website"
	    :components
	    ((:module "source"
		      :components ((:static-file "index.md"))))))

  :in-order-to ((test-op (load-op cl-markdown-test)))
  :perform (test-op :after (op c)
		    (funcall
		      (intern (symbol-name '#:run-tests) :lift)
		      :config :generic))
  :depends-on (:metatilities-base 
	       :metabang-bind
	       :dynamic-classes
	       :cl-containers
	       :anaphora
	       :cl-ppcre))

(defmethod operation-done-p 
           ((o test-op) (c (eql (find-system '#:cl-markdown))))
  (values nil))

