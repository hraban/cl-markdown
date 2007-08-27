(in-package #:cl-markdown)

(defun markdown-many (pairs &rest args 
		      &key format additional-extensions render-extensions
		      &allow-other-keys)
  (let ((main-document (make-instance 'document))
	(docs nil))
    (setf docs
	  (loop for datum in pairs collect
	       (bind (((source destination &rest doc-args) datum))
		 (format t "~&Parsing: ~s~%" source)
		 (list (apply #'markdown source
			      :parent main-document
			      :format :none (append doc-args args))
		       destination))))
    ;; transfer information from docs to the parent
    (loop for (doc destination) in docs do
	   (transfer-document-data main-document doc destination))
    ;; render 'em
    (loop for (doc destination) in docs do
	 (format t "~&Rendering: ~s" destination)
	 (let ((*current-document* doc)
	       (*render-active-functions*
		(mapcar #'canonize-command
			(or render-extensions
			    (if additional-extensions
				`(,@additional-extensions
				  ,@*render-active-functions*)
				*render-active-functions*)))))
	   (render-to-stream doc format destination)))
    (values main-document docs)))

(defun _render-one (doc)
  (let ((*current-document* doc)
	(*render-active-functions*
	 (mapcar #'canonize-command
		 `(cl-markdown::docs cl-markdown::docs-index
				     cl-markdown::today cl-markdown::now
				     cl-markdown::glossary
				     ,@*render-active-functions*))))
    (render-to-stream doc :html #p"/tmp/one.html")))

#+(or)
(untrace markdown)
    
#+(or)
(compile 'markdown-many)

#+(or)
(cl-markdown:markdown-many 
 `((,(system-relative-pathname 'cl-markdown "dev/md1.md") 
     ,(system-relative-pathname 'cl-markdown "dev/md1.html")) 
   (,(system-relative-pathname 'cl-markdown "dev/md2.md") 
     ,(system-relative-pathname 'cl-markdown "dev/md2.html")))
 :format :html)

(defun transfer-document-data (parent child destination)
  ;; links
  (let ((*current-document* parent))
    (iterate-key-value
     (link-info child)
     (lambda (id info)
       (setf (item-at (link-info parent) id)
	     (transfer-link-info info parent child destination))))
    ;; properties
    (iterate-key-value
     (properties child)
     (lambda (key value)
       (when (transfer-property-p child key)
	 (setf (document-property key) (first value)))))))

(defgeneric transfer-property-p (document property))

(defmethod transfer-property-p (document property) 
  (declare (ignore property document))
  (values t))

(defmethod transfer-property-p (document (property (eql :footnote)))
  (declare (ignore document))
  (values nil))

(defmethod transfer-property-p (document (property (eql :style-sheet)))
  (declare (ignore document))
  (values nil))

(defmethod transfer-property-p (document (property (eql :style-sheetshtml)))
  (declare (ignore document))
  (values nil))

(defmethod transfer-property-p (document (property (eql :title)))
  (declare (ignore document))
  (values nil))

(defgeneric transfer-link-info (info parent child destination))

(defmethod transfer-link-info ((info link-info) parent child destination)
  (declare (ignore parent child))
  (make-instance 'link-info
                 :id (id info)
		 :url (if (relative-url-p (url info))
			  (format nil "~a~@[.~a~]~a" 
				  (pathname-name destination)
				  (pathname-type destination)
				  (url info))
			  (url info))
		 :title (title info)))

(defun relative-url-p (url)
  ;; FIXME -- look at the spec...
  (not 
   (or (starts-with url "http:")
       (starts-with url "mailto:")
       (starts-with url "file:"))))
 
(defmethod transfer-link-info ((info extended-link-info)
			       parent child destination)
  (declare (ignore parent child destination))
  (make-instance 'extended-link-info
                 :id (id info)
		 :kind (kind info)
		 :contents (contents info)))


