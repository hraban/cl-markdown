(in-package #:cl-markdown)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(
	    symbols-exported-with-no-definition
	    symbols-that-should-be-documented
	    symbols-documented-by-document
	    symbols-documented-by-documents
	    symbols-not-documented-by-multi-document
	    )))

(defmethod documents ((document document)) (list document))

(defmethod documents ((document multi-document)) (children document))

(defun build-documentation-report (source target document packages
				   &key (format :html) excluded-symbols
				   docs-package search-locations)
  (cl-markdown:markdown
   source
   :format format
   :stream target
   :additional-extensions '(unmentioned-symbols-report
			    unused-exported-symbols-report
			    markdown-warnings-report
			    documented-symbols-report)
   :properties `((:documentation-document
		  . ,document)
		 (:documentation-documents
		  . ,(documents document))
		 (:documentation-packages
		  . ,(ensure-list packages))
		 (:documentation-excluded-symbols
		  . ,excluded-symbols)
		 ,@(when docs-package
			 `((:docs-package . ,(find-package docs-package))))
		 (:search-locations
		  . ,(append
		      (list
		       (system-relative-pathname 'cl-markdown "resources/"))
		      (ensure-list search-locations)))
		 (:style-sheet . "markdown-report-styles.css"))))

(defun symbols-defined-by-packages (packages &key excluded-symbols)
  (sort
   (remove-duplicates
    (loop for package in (ensure-list packages) append
	 (let ((package (find-package package)))
	   (loop for s being the symbols
	      of package when
	      (and (eql package (symbol-package s))
		   (or (fboundp s)
		       (boundp s)
		       (find-class s nil)
		       (get s 'prolog:functor)
		       (eq (system:variable-information s) :special)
		       (member s excluded-symbols)
		       #+allegro
		       (symbol-is-type-specifier-p s)
		       ;; Could check deftypes if necessary.
		       ))
	      collect s))))
   'string< :key 'symbol-name))

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

(defun symbols-documented-by-document (document default-package)
  (let ((*package* (or (let ((*current-document* document))
			 (document-property :docs-package))
		       (find-package default-package)))
	(markdown-package (find-package :cl-markdown)))
    (flet ((fix-symbol (symbol)
	     (if (eql (symbol-package symbol) markdown-package)
		 (intern (symbol-name symbol) *package*)
		 symbol)))
      (mapcar
       (lambda (thing)
	 (etypecase thing
	   (symbol (fix-symbol thing))
	   (cons (list (first thing) (fix-symbol (second thing))))))
       (cl-containers:collect-keys
	(first (cl-containers:item-at-1
		(cl-markdown::properties document) :documentation-anchors))
	:transform #'car)))))

(defun symbols-documented-by-documents (documents default-package)
  (remove-duplicates
   (loop for document in documents append
	(symbols-documented-by-document document default-package))))

(defun symbols-not-documented-by-multi-document
    (multi-doc packages default-package &key
     (excluded-packages (list :common-lisp
			      #+allegro :excl))
     (excluded-symbols nil))
  (sort
   (set-difference
    (symbols-that-should-be-documented
     packages :excluded-packages excluded-packages
     :excluded-symbols excluded-symbols)
    (symbols-documented-by-documents multi-doc default-package))
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
	     (let ((symbols (symbols-exported-with-no-definition
			     packages :excluded-symbols excluded)))
	       (format os "~&<div>~%")
	       (format os "~&<h2>From packages:</h2>~%")
	       (format os "~&~{~&<span>~a</span>~^ ~}"
		       (mapcar #'package-name packages))
	       (format os "~&</div>~%")
	       (cond ((> (length symbols) 0)
		      (format os "~&<h2>~:d Symbols</h2>~%"
			      (length symbols))
		      (loop for s in symbols do
			   (format os "~&<span>~s</span> ~%" s)))
		     (t
		      (format os "~&All exported symbols are accounted for~%"
			      (length symbols))))
	       (when excluded
		 (format os "~&<div class='excluded-symbols'>~%")
		 (format os "~&<h2>Ignoring the following ~:d symbols</h2>~%"
			 (length excluded))
		 (format os "~&~{<span class='excluded-symbol'>~s</span> ~^ ~%~}~%"
			 excluded)
		 (format os "~&</div>~%"))))
	    (t
	     (format os "~&<p>There are no packages specified by the
property :documentation-packages~%")))
      (format os "~&</div>~%"))))

(defextension (unmentioned-symbols-report
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
      (format os "~&<h1>Exported Symbols not mentioned in the documentation</h1>~%")
      (cond (packages
	     (let ((symbols (symbols-not-documented-by-multi-document
			     documents
			     packages
			     *package*
			     :excluded-symbols excluded)))
	       (format os "~&<div>~%")
	       (format os "~&<h2>From packages:</h2>~%")
	       (format os "~&~{~&<span>~a</span> ~^ ~}"
		       (mapcar #'package-name packages))
	       (format os "~&</div>~%")
	       (cond ((> (length symbols) 0)
		      (format os "~&<h2>~:d Undocumented Symbols</h2>~%"
			      (length symbols))
		      (loop for s in symbols
			 do (format os "~&<span>~s</span> ~%" s)))
		     (t
		      (format os "~&All exported symbols are documented.~%")))
	       (when excluded
		 (format os "~&<div class='excluded-symbols'>~%")
		 (format os "~&<h2>Ignoring ~d symbols</h2>~%"
			 (length excluded))
		 (unless donot-display-excluded-symbols
		   (format
		    os "~&~{<span class='excluded-symbol'>~s</span> ~^ ~%~}~%"
		    excluded))
		 (format os "~&</div>~%"))))
	    (t
	     (format os "~&<p>There are no packages specified by the
propoerty :documentation-packages~%")))
      (format os "~&</div>~%"))))

;; FIXME -- this is wrong (it's the same as unmentioned right now)
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
      (format os "~&<h1>Exported Symbols not mentioned in the documentation</h1>~%")
      (cond (packages
	     (let ((symbols (symbols-not-documented-by-multi-document
			     documents
			     packages
			     *package*
			     :excluded-symbols excluded)))
	       (format os "~&<div>~%")
	       (format os "~&<h2>From packages:</h2>~%")
	       (format os "~&~{~&<span>~a</span> ~^ ~}"
		       (mapcar #'package-name packages))
	       (format os "~&</div>~%")
	       (cond ((> (length symbols) 0)
		      (format os "~&<h2>~:d Undocumented Symbols</h2>~%"
			      (length symbols))
		      (loop for s in symbols
			 do (format os "~&<span>~s</span> ~%" s)))
		     (t
		      (format os "~&All exported symbols are documented.~%")))
	       (when excluded
		 (format os "~&<div class='excluded-symbols'>~%")
		 (format os "~&<h2>Ignoring ~d symbols</h2>~%"
			 (length excluded))
		 (unless donot-display-excluded-symbols
		   (format
		    os "~&~{<span class='excluded-symbol'>~s</span> ~^ ~%~}~%"
		    excluded))
		 (format os "~&</div>~%"))))
	    (t
	     (format os "~&<p>There are no packages specified by the
propoerty :documentation-packages~%")))
      (format os "~&</div>~%"))))

(defextension (documented-symbols-report)
  (when (eq phase :render)
    (let* ((documents (document-property :documentation-documents))
	   (*package* (or (document-property :docs-package)
			  *package*))
	   (os *output-stream*)
	   (symbols (symbols-documented-by-document
		     documents *package*)))
      (format os "~&<div id='documented-symbols-report' class='markdown-report'>~%")
      (format os "~&<h1>Documented symbols</h1>~%")
      (cond ((> (length symbols) 0)
	     (format os "~&<h2>~:d Documented Symbols</h2>~%"
		     (length symbols))
	     (loop for s in symbols
		do (format os "~&<span>~s</span> ~%" s)))
	    (t
	     (format os "~&All exported symbols are documented.~%")))
      (format os "~&</div>~%"))))

(defextension (markdown-warnings-report
	       :arguments ())
  (when (eq phase :render)
    (bind ((os *output-stream*)
	   (document (document-property :documentation-document))
	   (documents (merge-elements
		       (warnings document)
		       (lambda (old new)
			 (push new old))
		       (lambda (new)
			 (list new))
		       :key 'first :argument 'cdr
		       :filter (lambda (pair)
				 (typep (first pair) 'document #+(or) 'child-document))))
	  (warnings? nil))
      (format os "~&<div id='markdown-warnings' class='markdown-report'>~%")
      (format os "~&<h1>Markdown Warnings</h1>~%")

      (setf documents (sort documents #'string-lessp
		  :key (compose 'ensure-string 'source 'first)))
      (loop for (document warnings) in documents do
	   (when warnings
	     (setf warnings? t)
	     (format os "~&<div>~%")
	       (format os "~&<h2>~a</h2>~%"
		     (short-source (source document)))
	     (format os "~&<ul>~%")
	     (loop for warning in (merge-elements
				   warnings
				   (lambda (old new)
				     (declare (ignore new))
				     (incf (second old))
				     old)
				   (lambda (new) (list new 1))
				   :test 'equal
				   :return :values) do
		  (format os "~&<li>~a</li>~%" warning))
	     (format os "~&</ul>~%")
	     (format os "~&</div>~%")))
      (unless warnings?
	(format os "~%No warnings found.~%"))
      (format os "~&</div>~%"))))
