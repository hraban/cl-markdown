(in-package #:cl-markdown)

(defclass* document ()
  ((chunks (make-container 'vector-container) r)
   (link-info (make-container 'simple-associative-container
                              :test #'equalp) r)
   (level 0 a)
   (markup nil a)
   (properties (make-container 'alist-container
                               :test #'string-equal) r)
   (metadata (make-container 'alist-container
			     :test #'string-equal) r)
   (bracket-references (make-container 'flexible-vector-container) r)
   (parent nil ir)
   (warnings nil a)
   (source nil ir)))

(defgeneric document-property (name &optional default)
  (:documentation "Returns the value of the property `name` of the `*current-document*` or the default if the property is not defined or there is no `*current-document*`."))

(defmethod document-property (name &optional default)
  (or (when *current-document*
	(multiple-value-bind (value found?)
	    (item-at-1 (properties *current-document*) 
		       (form-property-name name))
	  (when found? (return-from document-property (first value)))))
      (when (and *current-document* 
		 (parent *current-document*))
	(let ((*current-document* (parent *current-document*)))
	  (document-property name default)))
      default))

(defmethod (setf document-property) (value name)
  (when *current-document*
    (setf (item-at-1 (properties *current-document*) 
		     (form-property-name name))
	  ;; so that we don't lose 'nil'
	  (list value)))
    ;;?? weird since nothing happened
    (values value))

(defun find-link (id)
  (or (item-at-1 (link-info *current-document*) id)
      (and (parent *current-document*)
	   (let ((*current-document* (parent *current-document*)))
	     (find-link id)))))

(defun form-property-name (name)
  (form-keyword (typecase name 
		  (string (intern name (find-package :keyword)))
		  (symbol (form-property-name (symbol-name name)))
		  (t name))))

(defclass* chunk ()
  ((lines (make-container 'vector-container) r)
   (blank-line-before? nil ia)
   (blank-line-after? nil ia)
   (started-by nil ia)
   (ended-by nil ia)
   (ignore? nil a)
   (markup-class nil ia)
   (indentation 0 ia)
   (level 0 ia)
   (paragraph? nil ia)
   (properties (make-container 'alist-container
                               :test #'string-equal) r)
   (stripper? nil ia)))

(defmethod initialize-instance :after ((object chunk) &key lines)
  (when lines
    (iterate-elements lines (lambda (line) (insert-item (lines object) line)))))

(defmethod print-object ((chunk chunk) stream)
  (print-unreadable-object (chunk stream :type t)
    (format stream "~A/~A ~D lines ~A ~A" 
            (markup-class chunk)
            (level chunk)
            (size (lines chunk))
            (started-by chunk)
            (ended-by chunk))))

(defclass* chunk-parsing-environment ()
 ((name nil ir)
   (chunk-enders nil ia)
   (chunk-starters nil ia)
   (line-coders nil ia)
   (parser-map nil ia)))

(defclass* parsing-environment ()
  ((chunk-parsing-environment (make-container 'stack-container) r)
   (chunk-post-processors nil ia)
   (chunk-level 0 ia)
   (current-strip "" ia)
   (line-code->stripper (make-container 'simple-associative-container
					:initial-element nil #+(or) 'null-stripper) r)
   (strippers (make-container 'stack-container) r)))
                 
(defun current-chunk-parser ()
  (first-item (chunk-parsing-environment *parsing-environment*)))

(defclass* basic-link-info ()
  ((id nil ia)))

(defclass* link-info (basic-link-info)
  ((url nil ir)
   (title nil ia)
   (properties nil ia)))

(defmethod initialize-instance :after ((link link-info) &key properties)
  (setf (properties link)
	(collect-window-over-elements
	  (string->list properties) 2 2  
	  :transform
	  (lambda (pair) (cons (form-keyword (first pair))
			       (second pair))))))

(defclass* extended-link-info (basic-link-info)
  ((kind nil ir)
   (contents nil ia)))

(defmethod print-object ((object link-info) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A -> ~A" (id object) (url object))))
       