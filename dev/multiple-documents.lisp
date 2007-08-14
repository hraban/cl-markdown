(in-package #:cl-markdown)

(defun markdown-many (pairs &rest args &key format &allow-other-keys)
  (let ((main-document (make-instance 'document))
	(docs nil))
    (setf docs
	  (loop for (source destination) in pairs collect
	       (list (apply #'markdown source
			    :parent main-document
			    :format :none args)
		     destination)))
    ;; transfer information from docs to the parent
    (loop for (doc destination) in docs do
	 (transfer-document-data main-document doc destination))
    ;; render 'em
    (loop for (doc destination) in docs do
	 (let ((*current-document* doc))
	   (render-to-stream doc format destination)))
    (values main-document docs)))

#+(or)
(markdown-many 
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
       (setf (document-property key) value)))))

(defgeneric transfer-link-info (info parent child destination))

(defmethod transfer-link-info ((info link-info) parent child destination)
  (declare (ignore parent child))
  (make-instance 'link-info
                 :id (id info)
		 :url (if (relative-url-p (url info))
			  (format nil "~a~a" destination (url info))
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


