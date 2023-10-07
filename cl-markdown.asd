(in-package #:common-lisp-user)

(defpackage #:cl-markdown-system (:use #:cl #:asdf))
(in-package #:cl-markdown-system)

;; Load asdf-system-connections if available
(unless (member :asdf-system-connections *features*)
  (if (asdf:find-system "asdf-system-connections" nil)
      (asdf:load-system "asdf-system-connections")
      (warn "The CL-Markdown system would enjoy having ~
asdf-system-connections around. See
http://www.cliki.net/asdf-system-connections for details and download
instructions.")))

(defsystem cl-markdown 
  :version "0.10.6"
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Hraban Luyat <hraban@0brg.net>"
  :licence "GPL-3.0-only"
  :components
  ((:static-file "COPYING")
   (:module "setup"
	    :pathname "dev/"
	    :components 
	    ((:file "package")
	     (:file "api"
		    :depends-on ("package"))))
   (:module "dev"
	    :depends-on ("setup")
	    :components
	    ((:file "definitions")
	     (:file "macros")
	     (:file "class-defs"
		    :depends-on ("definitions"))
	     (:file "utilities"
		    :depends-on ("macros" "definitions" "class-defs"))
	     (:file "spans"
		    :depends-on ("regexes" "class-defs"))
	     (:file "regexes")
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
  :depends-on ((:version :metatilities-base "0.6.0") 
	       :metabang-bind
	       ;; ugh, the order matters here. Add more duct tape
	       #-asdf-system-connections :container-dynamic-classes
	       (:version :cl-containers "0.11.5")
	       :dynamic-classes
	       :anaphora
	       :cl-ppcre))

(defmethod operation-done-p 
           ((o test-op) (c (eql (find-system '#:cl-markdown))))
  (values nil))

