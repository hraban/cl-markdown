(in-package #:cl-markdown)

(defun docs-package ()
  (let ((property (document-property :docs-package)))
    (typecase property
      (package property)
      (t (let ((package
		(or (find-package property) 
		    (find-package (string-upcase property)))))
	   (setf (document-property :docs-package) package))))))

(defun check-exportedp (symbol)
  (unless (eq (nth-value 1 (find-symbol (symbol-name symbol)
					(symbol-package symbol)))
	      :external)
    (markdown-warning "Symbol ~s is not exported" symbol)))  

(defun ensure-symbol (thing &optional (package nil))
  (etypecase thing
    (symbol (if (and package (not (eq (symbol-package thing) package)))
		(intern (symbol-name thing) package)
		thing))
    (string (intern thing package))))

(defextension (docs :arguments ((name) (kind)))
  (bind (symbol)
    (labels ((find-docs (thing)
	       (bind (((values kinds nil)
		       (aif (symbol-identities-with-docstring thing kind)
			    (values it t)
			    (values (mapcar (lambda (x) (cons x nil))
					    (symbol-identities thing)) nil))))
		 (setf symbol thing)
		 kinds)))
      (bind ((*package* (or (docs-package) *package*))
	     (kinds (or (find-docs (ensure-symbol name))
			(find-docs (ensure-symbol name *package*))))
	     (kind (first kinds)))
	(ecase phase 
	  (:parse
	   (check-exportedp symbol) 
	   ;;?? could memoize this (where is it stored? in add-docs-item?)
	   (when (> (length kinds) 1)
	     (markdown-warning "Multiple interpretations found for ~s; specify type (using ~a)" 
			  name (car (first kinds))))
	   (unless kinds
	     (markdown-warning "No docstring found for ~s (package is ~s)"
			  name (docs-package)))
	   (add-docs-item symbol (car kind)))
	  (:render
	   (let ((docs (and (cdr kind) (find-documentation symbol (cdr kind))))
		 (identity (car kind)))
	     (maybe-anchor-documentation name identity)
	     (format *output-stream* 
		     "<div class=\"documentation ~(~a~)\">" identity)
	     (format *output-stream* 
		     "<div class=\"documentation header\">")
	     (format *output-stream* "<div class=\"doc name-and-args\">")
	     (format *output-stream* "<span class=\"hidden\">X</span>")
	     (format *output-stream*
		     "~&<span class=\"documentation-name\">~s</span>" symbol)
	     (when (symbol-may-have-arguments-p symbol)
	       (let ((arguments (mopu:function-arglist symbol)))
		 (when arguments
		   (format *output-stream*
			   "~&<span class=\"documentation-arguments\">")
		   (display-arguments arguments :kind identity)
		   (format *output-stream* "</span>"))))
	     (format *output-stream* "~&</div>~%")
	     (format *output-stream* 
		     "~&<span class=\"documentation-kind\">~a</span>" identity)
	     (format *output-stream* "~&</div>~%")
	     (format *output-stream* 
		     "<div class=\"documentation contents\">")	   
	     (cond 
	       (docs
		(markdown docs
			  :stream *output-stream*
			  :format *current-format*
			  :properties '(("html" . nil)
					(:omit-final-paragraph . t)
					(:omit-initial-paragraph . t))))
	       (t
		(format *output-stream* 
			"<span class='no-docs'>No documentation found</span>")))
	     (format *output-stream* "~&</div>~%")
	     (format *output-stream* "~&</div>~%"))
	   nil))))))

(defun maybe-anchor-documentation (name identity)
  (unless (documentation-anchored-p name identity)
    (setf (documentation-anchored-p name identity) t)
    (output-anchor (format nil "~a.~a" name identity))
    (output-anchor name)))

(defun documentation-anchors-table ()
  (or (document-property :documentation-anchors)
      (setf (document-property :documentation-anchors)
	    (make-container 'simple-associative-container :test 'equal))))

(defun documentation-anchored-p (name identity)
  (item-at-1 (documentation-anchors-table)
	     (cons name identity)))

(defun (setf documentation-anchored-p) (value name identity)
  (setf (item-at-1 (documentation-anchors-table)
		   (cons name identity))
	value))

(defun output-documentation-link (item kind text)
  (let ((name (html-safe-name (format nil "~a.~a" item kind))))
    (format *output-stream*
	    "~&<li><a href=\"#~a\">\~a</a></li>" 
	    name text)))

(defextension (docs-index :arguments ((kind-or-kinds :required)
				      index-kind))
  (setf kind-or-kinds (ensure-list kind-or-kinds))
  (unless index-kind
    (setf index-kind (first kind-or-kinds)))
  (when (eq phase :render)
    (bind ((items (%items-to-index kind-or-kinds)))
      (cond ((empty-p items)
	     (markdown-warning 
	      (if (length-1-list-p kind-or-kinds)
		  "There are no items of kind ~{~a~} documented."
		  "There are no itmes of kinds~{ ~a~^ or~} documented")
	      kind-or-kinds))
	    (t
	     (output-anchor index-kind)
	     (format *output-stream* 
		     "~&<div class=\"index ~(~a~)\">" index-kind)
	     (format *output-stream* "~&<ul>")
	     (loop for (item . real-kind) in items do
		  (output-documentation-link item real-kind item))
	     (format *output-stream* "~&</ul></div>"))))))

(defun canonize-index-kind (kind)
  (intern (string-downcase (ensure-string kind))
	  (load-time-value (find-package :cl-markdown))))

(defun %items-to-index (kinds)
  (let ((docs (item-at-1 (metadata *current-document*) :docs))
	(kinds (mapcar #'canonize-index-kind kinds)))
    (sort 
     (cond ((member 'all kinds)
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
	    (loop for kind in kinds nconc
		 (collect-keys (item-at-1 docs kind) 
			       :transform (lambda (symbol)
					    (cons symbol kind))))))
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
	     (let ((anchor (html-safe-name (ensure-string name))))
	       (setf (item-at (link-info *current-document*) name)
		     (make-instance 'link-info
				    :id name :url (format nil "#~a" anchor) 
				    :title title)))))
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

#|
If there is a suppliedp variable for an optional or keyword arg, it should
not be printed.

If there is an initial value for an optional or keyword, it should be
printed with ~S otherwise 'foo :foo "foo" all print as foo in the doc.

If the initial value is a big expression, it should not print in the doc.
The effect of that big expression should be explained in words.

If the initial value is nil, it does not need to show in the argument line.
|#

#|
(defun xxxfoo     (triple-count index-count 
     &key (skip-size (ag-property 'default-metaindex-skip-size))
     (unique-strings (floor triple-count 3))
     (average-string-size nil))
  )

(let ((*output-stream* *standard-output*))
  (display-arguments (mopu:function-arglist 'lift:ensure-cases) :kind 'macro))

(trace display-arguments)
(untrace display-arguments)
|#

(defun display-arguments (arguments &key (kind nil))
  ;; kind can be anything returned by symbol-identities
  ;; currently, we only care about macros
  (let ((space-entity (document-property "docs-space-entity" "&ensp;"))
	(first? t))
    (dolist (argument arguments)
      ;; bail on &aux
      (when (and (symbolp argument)
		 (string-equal (symbol-name argument) "&aux"))
	(return))
      (unless first?
	(format *output-stream* "~a" space-entity))
      ;; dispatch?
      (cond ((consp argument)
	     (case kind
	       (macro
		(format *output-stream* "(")
		(display-arguments argument :kind kind)
		(format *output-stream* ")"))
	       (t
		(cond ((eq (length argument) 3)
		       ;; (name initform supplied)
		       ;; ((:key name) initform supplied)
		       ;; just recur without the supplied
		       (display-arguments (list (butlast argument)) :kind kind))
		      ((eq (length argument) 2)
		       (if (consp (car argument))
			   ;; ((:key name) initform)
			   (bind ((((key name) initform) argument)
				  (package (symbol-package name))
				  (new-name (intern (symbol-name key) package)))
			     (display-arguments (list (list new-name initform)) 
					       :kind kind))
			   ;; (name initform)
			   (bind (((name initform) argument))
			     (cond ((null initform)
				    ;; just show argument
				    (format *output-stream* "~(~s~)" name))
				   ((constantp initform)
				    ;; show both
				    (format *output-stream* "~((~s ~s)~)"
					    name initform))
				   (t
				    ;; just show name
				    (format *output-stream* "~(~s~)" name))))))
		      (t
		       ;; probably part of a macro
		       (format *output-stream* "(")
		       (display-arguments argument :kind kind)
		       (format *output-stream* ")"))))))
	    ((and (symbolp argument)
		  (string-equal (symbol-name argument) "&" 
				:start1 0 :start2 0 :end1 1 :end2 1)) 
	     (format *output-stream* 
		     "<span class=\"marker\">&amp;~(~a~)</span>" 
		     (subseq (symbol-name argument) 1)))
	    (t
	     (format *output-stream* "~(~s~)" argument)))
      (setf first? nil))))

(defgeneric find-documentation (thing strategy)
  (:documentation "Return the documentation for thing using strategy. The default is to call the Common Lisp documentation method with strategy being used as the type."))

(defmethod find-documentation (thing strategy)
  (documentation thing strategy))

(defmethod find-documentation (thing (strategy (eql 'function)))
  (cond ((and (fboundp thing)
	      (typep (symbol-function thing) 'standard-generic-function))
	 (let ((docstring (call-next-method))
	       (strings 
		(loop for m in (mopu:generic-function-methods
				(symbol-function thing)) 
		   when (documentation m 'function) append
		     (list (documentation m 'function)))))
	   (format nil "~@[~a~]~:[~;~%~%~]~@[~{~a~^~%~%~}~]" 
		   docstring (and docstring strings) strings)))
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

(defextension (links-list :arguments ((kind-or-kinds :required)
				      index-kind))
  (setf kind-or-kinds (ensure-list kind-or-kinds))
  (unless index-kind
    (setf index-kind (first kind-or-kinds)))
  (when (eq phase :render)
    (bind ((items (%items-to-index kind-or-kinds)))
      (cond ((empty-p items)
	     (markdown-warning 
	      (if (length-1-list-p kind-or-kinds)
		  "There are no items of kind ~{~a~} documented."
		  "There are no itmes of kinds~{ ~a~^ or~} documented")
	      kind-or-kinds))
	    (t
	     (format *output-stream* 
		     "~&(" index-kind)
	     (loop for (item . real-kind) in items do
		  (output-links-list-item item real-kind item)))))))

(defun output-links-list-item (item kind text)
  (let ((name (html-safe-name (format nil "~a.~a" item kind))))
    (format *output-stream*
	    "~&(#~a ~a)" 
	    name text)))
