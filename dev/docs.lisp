(in-package #:cl-markdown)

(defun docs-package ()
  (let ((property (document-property :docs-package)))
    (typecase property
      (package property)
      (t (let ((package
		(or (find-package property) 
		    (find-package (string-upcase property)))))
	   (setf (document-property :docs-package) package))))))

(defun ensure-symbol (thing &optional (package *package*))
  (etypecase thing
    (symbol (if (eq (symbol-package thing) package)
		thing
		(intern (symbol-name thing) package)))
    (string (intern thing package))))

(defun kind-mappings (kind)
  (case (form-keyword kind)
    (:macro '(macro function))
    (t kind)))
     
(defextension (docs :arguments ((name) (kind)))
  (bind ((*package* (or (docs-package) *package*))
	 (thing (ensure-symbol name *package*))
	 (kind-or-kinds 
	  (loop for kind in (ensure-list
			     (or (kind-mappings (and kind (ensure-symbol kind)))
				 '(function variable package setf
				   type structure compiler-macro
				   method-combination t)))
	     when (documentation thing kind) collect
	     kind))
	 (kind (when (consp kind-or-kinds) (first kind-or-kinds))))
    (ecase phase 
      (:parse
       ;;?? could memoize this
       (when (consp kind-or-kinds)
	 (when (> (length kind-or-kinds) 1)
	   (warn "Multiple docstrings found for ~s; specify type (using ~a)" 
		 name (first kind-or-kinds)))
	 (setf kind-or-kinds (first kind-or-kinds)))
       (when (and (consp kind-or-kinds) (> (length kind-or-kinds) 1))
	 (setf kind-or-kinds (first kind-or-kinds))
	 (warn "Multiple docstrings found for ~s; specify type (using ~a)" 
	       name kind-or-kinds))
       (unless kind-or-kinds
	 (warn "No docstring found for ~s" name))
       (add-docs-item thing kind-or-kinds))
      (:render
       (let ((docs (documentation thing kind)))
	 (format *output-stream* 
		 "~&<a name=\"~a-~a\" id=\"~a-~a\"></a>" name kind name kind)
	 (format *output-stream* 
		 "<div class=\"documentation ~(~a~)\">" kind)
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
		 "~&<span class=\"documentation-kind\">~a</span>" kind)
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
       nil))))

(defextension (docs-index :arguments ((kind :required) (sorted :keyword)))
  (when (eq phase :render)
    (bind ((item-table (item-at-1 
			(item-at-1 (metadata *current-document*) :docs) kind)))
      (if (empty-p item-table)
	  (warn "There are no items of kind ~a documented." kind)
	  (bind ((items (collect-keys item-table)))
	    (when sorted (setf items (sort items #'string-lessp)))
	    (format *output-stream* 
		    "~&<a href=\"index-~a\"></a>" kind)
	    (format *output-stream* 
		    "~&<div class=\"index ~(~a~)\">" kind)
	    (format *output-stream* "~&<ul>")
	    (loop for item in items do
		 (format *output-stream* "~&<li><a href=\"#~a-~a\">\~a</a></li>" 
			 item kind item))
	    (format *output-stream* "~&</ul></div>"))))))

(defun add-docs-item (thing kind)
  (add-docs-link thing kind)
  (bind ((docs-items
	  (or (item-at-1 (metadata *current-document*) :docs)
	      (setf (item-at-1 (metadata *current-document*) :docs)
		    (make-container 'simple-associative-container))))
	 (kind-items
	  (or (item-at-1 docs-items kind)
	      (setf (item-at-1 docs-items kind)
		    (make-container 'simple-associative-container)))))
    (setf (item-at-1 kind-items thing) t)))

(defun add-docs-link (thing kind)
  (bind ((kinds (symbol-identities thing))
	 ((values name title)
	  (if (length-1-list-p kinds)
	      (values (format nil "~a" thing) 
		      (format nil "description of ~a" thing))
	      (values (format nil "~a.~a" kind thing)
 		      (format nil "description of ~a ~a" kind thing)))))
    (setf (item-at (link-info *current-document*) name)
	  (make-instance 'link-info
			 :id name :url (format nil "#~a" name) 
			 :title title))))
    
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

(defun symbol-identities (symbol)
  (loop for (predicate . kind) in 
       '((symbol-names-class-p . class)
	 (symbol-names-condition-p . condition)
	 (symbol-names-constant-p . constant)
	 (symbol-names-function-p . function)
	 (symbol-names-generic-function-p . generic-function)
	 (symbol-names-macro-p . macro)
	 (symbol-names-variable-p . variable)) 
       when (funcall predicate symbol) collect kind))

(defun symbol-names-class-p (symbol)
  (let ((class (find-class symbol nil))) 
    (and class 
	 (typep class 'standard-class)
	 (not (conditionp class)))))

;; FIXME -- this won't work on Lisps that don't use CLOS
;; for this conditions
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
