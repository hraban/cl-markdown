(in-package #:cl-markdown)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(
	    symbols-exported-with-no-definition
	    symbols-that-should-be-documented
	    symbols-documented-by-document 
	    symbols-documented-by-documents
	    symbols-not-documented-by-multi-document 
	    )))

(defun symbols-exported-with-no-definition (packages &key excluded-symbols)
 (sort 
  (remove-duplicates
   (loop for package in (ensure-list packages) append
	(loop for s being the external-symbols
	   of package unless 
	   (or (fboundp s)
	       (boundp s)
	       (find-class s nil)
	       (get s 'prolog:functor)
	       (eq (system:variable-information s) :special)
	       (member s excluded-symbols)
	       #+allegro
	       (symbol-is-type-specifier-p s)
	       ;; Could check deftypes if necessary.
	       )
	   collect s)))
  'string< :key 'symbol-name))

#+allegro
(defun symbol-is-type-specifier-p (x)
  (or (excl:normalize-type 
       x :loud (lambda (&rest r)
		 (declare (ignore r))
		 (return-from symbol-is-type-specifier-p nil)))
      t))

(defun symbols-that-should-be-documented 
    (packages &key 
     (excluded-packages (list :common-lisp
			      #+allegro :excl))
     excluded-symbols)
  (let ((excluded-packages (mapcar #'find-package excluded-packages)))
    (sort 
     (remove-duplicates
      (loop for package in (ensure-list packages) append
	   (loop for s being the external-symbols
	      of package unless 
	      (or (member s excluded-symbols)
		  (member (symbol-package s) excluded-packages))
	      collect s)))
     'string< :key 'symbol-name)))

#+(or)
(mapcar #'print
 (symbols-that-should-be-documented 
  (list :db.agraph :db.agraph.parser
	:db.agraph.serializer
	:sparql
	:net.cluster)))

(defun symbols-documented-by-document 
    (document &key (default-package :db.agraph))
  (let ((*package* (find-package default-package))
	(markdown-package (find-package :cl-markdown)))
    (mapcar
     (lambda (symbol)
       (if (eql (symbol-package symbol) markdown-package)
	   (intern (symbol-name symbol) *package*)
	   symbol))
     (cl-containers:collect-keys
      (first (cl-containers:item-at-1 
	      (cl-markdown::properties document) :documentation-anchors)) 
      :transform #'car))))

(defun symbols-documented-by-documents (documents)
  (remove-duplicates
   (loop for document in documents append
	(symbols-documented-by-document document))))

(defun symbols-not-documented-by-multi-document 
    (multi-doc packages &key
     (excluded-packages (list :common-lisp
			      #+allegro :excl))
     (excluded-symbols nil))
  (sort 
   (set-difference 
    (symbols-that-should-be-documented
     packages :excluded-packages excluded-packages
     :excluded-symbols excluded-symbols) 
    (symbols-documented-by-documents multi-doc))
   #'string-lessp))
 
#+ignore
(symbols-not-documented-by-multi-document 
 (second *last-multi-doc*)
 (list :db.agraph :db.agraph.parser
       :db.agraph.serializer
       :sparql
       :net.cluster)
 :excluded-symbols (symbols-explicitly-undocumented-for-agraph))

(defextension (unused-exported-symbols-report)
  (when (eq phase :render)
    (let ((packages (document-property :documentation-packages))
	  (excluded (document-property :documentation-excluded-symbols))
	  (*package* (or (document-property :docs-package)
			 *package*))
	  (os *output-stream*))
      (format os "~&<div id='symbols-exported-with-no-definition' class='markdown-report'>~%")
      (format os "~&<h1>Exported Symbols with no apparent use</h1>~%")
      (cond (packages
	     (format os "~&<div>~%")
	     (format os "~&<h2>From packages:</h2>~%")
	     (format os "~&~{~&<span>~a</span>~^~}"
		     (mapcar #'package-name packages))
	     (format os "~&</div>~%")
	     (format os "~&<h2>Symbols</h2>~%")
	     (loop for s in (symbols-exported-with-no-definition
			     packages :excluded-symbols excluded)
		do (format os "~&<span>~s</span>~%" s))
	     (when excluded
	       (format os "~&<div class='excluded-symbols'>~%")
	       (format os "~&<h2>Ignoring the following symbols</h2>~%")
	       (format os "~&~{<span class='excluded-symbol'>~s</span>~^~%~}~%"
		       excluded)
	       (format os "~&</div>~%")))
	    (t
	     (format os "~&<p>There are no packages specified by the 
property :documentation-packages~%")))
      (format os "~&</div>~%"))))

(defextension (undocumented-symbols-report
	       :arguments ((packages :keyword)
			   (excluded-symbols :keyword)
			   (donot-display-excluded-symbols :keyword)))
  (when (eq phase :render)
    (let ((packages (or packages
			(document-property :documentation-packages)))
	  (excluded (or excluded-symbols
			(document-property :documentation-excluded-symbols)))
	  (documents (document-property :documentation-documents))
	  (*package* (or (document-property :docs-package)
			 *package*))
	  (os *output-stream*))
      (format os "~&<div id='symbols-not-documented' class='markdown-report'>~%")
      (format os "~&<h1>Symbols that are not documented</h1>~%")
      (cond (packages
	     (format os "~&<div>~%")
	     (format os "~&<h2>From packages:</h2>~%")
	     (format os "~&~{~&<span>~a</span>~^ ~}"
		     (mapcar #'package-name packages))
	     (format os "~&</div>~%")
	     (format os "~&<h2>Symbols</h2>~%")
	     (loop for s in (symbols-not-documented-by-multi-document
			     documents
			     packages
			     :excluded-symbols excluded)
		do (format os "~&<span>~s</span>~%" s))
	     (when excluded
	       (format os "~&<div class='excluded-symbols'>~%")
	       (format os "~&<h2>Ignoring ~d symbols</h2>~%"
		       (length excluded))
	       (unless donot-display-excluded-symbols
		 (format 
		  os "~&~{<span class='excluded-symbol'>~s</span>~^~%~}~%"
		  excluded))
	       (format os "~&</div>~%")))
	    (t
	     (format os "~&<p>There are no packages specified by the 
propoerty :documentation-packages~%")))
      (format os "~&</div>~%"))))

(defextension (markdown-warnings-report
	       :arguments ())
  (when (eq phase :render)
    (let ((os *output-stream*)
	  (documents (document-property :documentation-documents))
	  (warnings? nil))
      (format os "~&<div id='markdown-warnings' class='markdown-report'>~%")
      (format os "~&<h1>Markdown Warnings</h1>~%")

      (loop for document in (ensure-list documents) do
	   (when (warnings document)
	     (setf warnings? t)
	     (format os "~&<div>~%")
	     (let ((source (source document)))
	       (format os "~&<h2>~a</h2>~%"
		       (typecase source
			 (pathname source)
			 (string 
			  (format nil "~a~@[...~]"
				  (subseq source 0 (min 50 (length source)))
				  (> (length source) 50)))
			 (t (format nil "Something of type ~s"
				    (type-of source))))))
	     (format os "~&<ul>~%")
	     (loop for warning in (warnings document) do
		  (format os "~&<li>~a</li>~%" warning))
	     (format os "~&</ul>~%")
	     (format os "~&</div>~%")))
      (unless warnings?
	(format os "~%No warnings found.~%"))
      (format os "~&</div>~%"))))
