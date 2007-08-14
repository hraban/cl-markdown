(in-package #:cl-markdown)

(defun docs-package ()
  (let ((property (document-property :docs-package)))
    (typecase property
      (package property)
      (t (let ((package
		(or (find-package property) 
		    (find-package (string-upcase property)))))
	   (setf (document-property :docs-package) package))))))

(defun ensure-symbol (thing &optional (package nil))
  (etypecase thing
    (symbol (if (and package (not (eq (symbol-package thing) package)))
		(intern (symbol-name thing) package)
		thing))
    (string (intern thing package))))

(defun doc-warning (msg &rest args)
  (let ((*print-readably* nil))
    (fresh-line *debug-io*)
    (apply #'format *debug-io* msg args)
    (terpri *debug-io*)))

(defextension (docs :arguments ((name) (kind)))
  (labels ((find-docs (thing)
	     (bind (((values kinds nil)
		     (aif (symbol-identities-with-docstring thing kind)
			  (values it t)
			  (values (mapcar (lambda (x) (cons x nil))
					  (symbol-identities thing)) nil))))
	       (values kinds thing))))
    (bind ((*package* (or (docs-package) *package*))
	   ((values kinds thing)
	    (or (find-docs (ensure-symbol name))
		(find-docs (ensure-symbol name *package*))))
	   (kind (first kinds)))		      
      (ecase phase 
	(:parse
	 ;;?? could memoize this (where is it stored? in add-docs-item?)
	 (when (> (length kinds) 1)
	   (doc-warning "Multiple interpretations found for ~s; specify type (using ~a)" 
		 name (car (first kinds))))
	 (unless kinds
	   (doc-warning "No docstring found for ~s (package is ~s)"
		 name (docs-package)))
	 (add-docs-item thing (car kind)))
	(:render
	 (let ((docs (and (cdr kind) (documentation thing (cdr kind))))
	       (identity (car kind)))
	   (format *output-stream* 
		   "~&<a name=\"~a-~a\" id=\"~a-~a\"></a>" 
		   name identity name identity)
	   (format *output-stream* 
		   "~&<a name=\"~a\" id=\"~a\"></a>" 
		   name name)
	   (format *output-stream* 
		   "<div class=\"documentation ~(~a~)\">" identity)
	   (format *output-stream* 
		   "<div class=\"documentation header\">")
	   (format *output-stream* "<span class=\"hidden\">X</span>")
	   (format *output-stream*
		   "~&<span class=\"documentation-name\">~a</span>" name)
	   (when (symbol-may-have-arguments-p thing)
	     (format *output-stream*
		     "~&<span class=\"documentation-arguments\">")
	     (display-arguments (mopu:function-arglist thing))
	     (format *output-stream*
		     "</span>"))
	   (format *output-stream* 
		   "~&<span class=\"documentation-kind\">~a</span>" identity)
	   (format *output-stream* "~&</div>")
	   (format *output-stream* 
		   "<div class=\"documentation contents\">")	   
	   (cond 
	     (docs
	      (markdown docs
			:stream *output-stream*
			:format *current-format*))
	     (t
	      (format *output-stream* 
		      "<span class='no-docs'>No documentation found</span>")))
	   (format *output-stream* "~&</div>")
	   (format *output-stream* "~&</div>"))
	 nil)))))

(defextension (docs-index :arguments ((kind :required)))
  (when (eq phase :render)
    (bind ((items (%items-to-index kind)))
      (cond ((empty-p items)
	     (doc-warning "There are no items of kind ~a documented." kind))
	    (t
	     (format *output-stream* 
		     "~&<a href=\"index-~a\"></a>" kind)
	     (format *output-stream* 
		     "~&<div class=\"index ~(~a~)\">" kind)
	     (format *output-stream* "~&<ul>")
	     (loop for (item . real-kind) in items do
		  (format *output-stream*
			  "~&<li><a href=\"#~a-~a\">\~a</a></li>" 
			  item real-kind item))
	     (format *output-stream* "~&</ul></div>"))))))

(defun canonize-index-kind (kind)
  (intern (string-downcase (ensure-string kind))
	  (load-time-value (find-package :cl-markdown))))

(defun %items-to-index (kind)
  (let ((docs (item-at-1 (metadata *current-document*) :docs))
	(kind (canonize-index-kind kind)))
    (sort 
     (cond ((eq kind :all)
	    (let ((result nil))
	      (iterate-key-value
	       docs
	       (lambda (kind item-table)
		 (setf result
		       (nconc result 
			      (collect-keys 
			       item-table
			       :transform (lambda (symbol)
					    (cons symbol kind)))))))
	      result))
	   (t
	    (collect-keys (item-at-1 docs kind) 
			  :transform (lambda (symbol)
				       (cons symbol kind)))))
     #'string-lessp
     :key #'first)))

(defun add-docs-item (thing kind)
  (let ((kind (canonize-index-kind kind)))
    (add-docs-link thing kind)
    (bind ((docs-items
	    (or (item-at-1 (metadata *current-document*) :docs)
		(setf (item-at-1 (metadata *current-document*) :docs)
		      (make-container 'simple-associative-container))))
	   (kind-items
	    (or (item-at-1 docs-items kind)
		(setf (item-at-1 docs-items kind)
		      (make-container 'simple-associative-container)))))
      (setf (item-at-1 kind-items thing) t))))

(defun add-docs-link (thing kind)
  (let ((kind (canonize-index-kind kind)))
    (flet ((add-link (name title)
	     (setf (item-at (link-info *current-document*) name)
		   (make-instance 'link-info
				  :id name :url (format nil "#~a" name) 
				  :title title))))
      (bind ((kinds (symbol-identities thing)))
	(cond ((length-1-list-p kinds)
	       (add-link (format nil "~a" thing) 
			 (format nil "description of ~a" thing)))
	      (t
	       (add-link (format nil "~a" thing) 
			 (format nil "description of ~a" thing))
	       (add-link (format nil "~a.~a" kind thing)
			 (format nil "description of ~a ~a" kind thing))))))))
    
(defun symbol-may-have-arguments-p (symbol)
  (and (fboundp symbol)
       (or 
	(typep (symbol-function symbol) 'function)
	(macro-function symbol)
	(typep (symbol-function symbol) 'standard-generic-function))))

(defun display-arguments (arguments)
  (dolist (argument arguments)
    (cond ((consp argument)
           ;; probably part of a macro
           (format *output-stream* "~( ~a~)" argument))
          ((string-equal (symbol-name argument) "&" 
			 :start1 0 :start2 0 :end1 1 :end2 1) 
           (format *output-stream* "~( ~a~)" argument))
          (t
           (format *output-stream* "~( ~a~)" argument)))))

(defgeneric find-documentation (thing strategy)
  (:documentation "Return the documentation for thing using strategy. The default is to call the Common Lisp documentation method with strategy being used as the type."))

(defmethod find-documentation (thing strategy)
  (documentation thing strategy))

(defmethod find-documentation (thing (strategy (eql 'function)))
  (cond ((and (fboundp thing)
	      (typep (symbol-function thing) 'standard-generic-function))
	 (let ((docstring (call-next-method))
	       (strings 
		(loop for m in (mop:generic-function-methods
				(symbol-function thing)) 
		   when (documentation m 'function) append
		     (list (documentation m 'function)))))
	   (format nil "~a~@[~%~%~{~a~^~%~%~}~]" docstring strings)))
	(t
	 (call-next-method))))

(defparameter *symbol-identities*
  '((symbol-names-class-p class type)
    (symbol-names-condition-p condition nil)
    (symbol-names-constant-p constant variable)
    (symbol-names-function-p function nil)
    ;; FIXME - for now, we don't separate 'em
    (symbol-names-generic-function-p function function)
    (symbol-names-macro-p macro function)
    (symbol-names-variable-p variable nil)
    (symbol-names-slot-accessor-p function function)))

(defun add-documentation-strategy (test thing strategy)
  (pushnew (list test thing strategy) *symbol-identities* 
	   :test #'equal
	   :key #'first))
 
(defun kind-mappings (kind)
  "Some kinds of things have their docstrings in 'other' places. For example,
macros put their docstrings under 'function. This function papers over the
distinction."
  (case (form-keyword kind)
    ((:macro :generic-function) '(function))
    (:class '(type))
    (:constant '(variable))
    (t kind)))

;; FIXME - fully reconcile list of docstring with list of identities
(defun symbol-identities-with-docstring (symbol &optional expected-kind)
  (let ((kinds 
	 (loop for kind in (ensure-list
			    (or (and expected-kind
				     (ensure-symbol expected-kind))
				(symbol-identities symbol))) 
	      for mappings = (kind-mappings kind)
	      for docs = nil
	    when
	      (or (and (atom mappings)
		       (find-documentation symbol mappings)
		       (setf docs mappings))
		  (and (consp mappings)
		       (some (lambda (doc-kind)
			       (and (find-documentation symbol doc-kind)
				    (setf docs doc-kind)))
			     mappings))) collect
	    (cons kind docs))))
    ;; FIXME -- I've got that adhoc feeling... priority?
    (when (find 'macro kinds :key #'car)
      (setf kinds (delete 'function kinds :key #'car)))
    (when (find 'constant kinds :key #'car)
      (setf kinds (delete 'variable kinds :key #'car)))
    kinds))

(defun symbol-identities (symbol)
  ;; cf. *symbol-identities*
  (loop for (predicate kind nil) in *symbol-identities* 
       when (funcall predicate symbol) collect kind))

(defun symbol-names-class-p (symbol)
  (let ((class (find-class symbol nil))) 
    (and class 
	 (typep class 'standard-class)
	 (not (conditionp class)))))

;; FIXME -- this (most likely) won't work on Lisps that don't use CLOS
;; for conditions
(defun  conditionp (thing)
  "Returns true if and only if thing is a condition"
  (mopu:subclassp thing 'condition))

(defun symbol-names-condition-p (symbol)
  (aand (find-class symbol nil)
	(conditionp it)))

(defun symbol-names-constant-p (symbol)
  (and (boundp symbol)
       (constantp symbol)))

(defun symbol-names-function-p (symbol)
  (and (fboundp symbol)
       (not (macro-function symbol))
       (typep (symbol-function symbol) 'function)
       (not (typep (symbol-function symbol) 'standard-generic-function))))

(defun symbol-names-generic-function-p (symbol)
  (and (fboundp symbol)
       (typep (symbol-function symbol) 'standard-generic-function)
       (some (lambda (m)
	       (not (or (mopu:reader-method-p m)
			(mopu:writer-method-p m))))
	     (mopu:generic-function-methods (symbol-function symbol)))))

(defun symbol-names-macro-p (symbol)
  (and (macro-function symbol)))

(defun symbol-names-variable-p (symbol)
  (and (boundp symbol)
       (not (constantp symbol))))

(defun symbol-names-slot-accessor-p (symbol)
  (and (fboundp symbol)
       (typep (symbol-function symbol) 'standard-generic-function)
       (some (lambda (m)
	       (or (mopu:reader-method-p m)
		   (mopu:writer-method-p m)))
	     (mopu:generic-function-methods (symbol-function symbol)))))
