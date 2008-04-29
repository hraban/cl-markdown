(in-package #:cl-markdown)

;; {f a0 .. an} 
;; -> eval f a0 .. an  -- where ai are strings
;; -> returns string that is inserted into document
;; -> or nil (cound do insertions itself)

;; no recursive function embeddings {date-stamp {today}}
;; keywords handled separately?

;; could use a macro
;;
;; to specify a name, arguments, etc and use that to parse. and export

(defsimple-extension today 
  (let ((format (document-property :date-format "%e %B %Y")))
    (format-date format (get-universal-time))))

(defsimple-extension now 
  (let ((format (document-property :time-format "%H:%M")))
    (format-date format (get-universal-time))))

(defextension (comment :arguments ((text :required)) :insertp t)
  (ecase phase
    (:parse
     ;; no worries
     )
    (:render 
     (format nil "<!-- ~a -->" text))))

(defextension (remark :arguments ((text :required)))
  (ecase phase
    (:parse
     ;; no worries
     )
    (:render 
     ;; stil no worries
     )))

(defextension (anchor :arguments ((name :required) title) :insertp t)
  (setf name (ensure-string name))
  (ecase phase
    (:parse
     (setf (item-at (link-info *current-document*) name)
	   (make-instance 'link-info
			  :id name :url (format nil "#~a" name) 
			  :title (or title ""))))
    (:render 
     (format nil "<a name='~a' id='~a'></a>" name name))))

(defextension (property :arguments ((name :required)))
  (ecase phase
    (:parse)
    (:render
     (process-child-markdown (document-property name))
     #+(or)
     (prog1
	 (strip-whitespace 
	  (nth-value 1 (markdown (document-property name) 
				 :parent *current-document*
				 :format *current-format*
				 :properties '((:omit-initial-paragraph t)
					       (:omit-final-paragraph t)
					       (:html . nil))
				 :stream nil)))))))

(defextension (ifdef :arguments ((keys :required) 
				 (text :required :whole)))
  (ecase phase
    (:parse)
    (:render
     (prog1
	 (if (or (and (atom keys) (document-property keys))
		 )
	     (process-child-markdown 
	      (format nil "~{~a~^ ~}" (ensure-list text)))
	     #+(or)
	     (strip-whitespace 
	      (nth-value 1 (markdown (format nil "~{~a~^ ~}" (ensure-list text))
				     :parent *current-document*
				     :format *current-format*
				     :properties '((:omit-initial-paragraph t)
						   (:omit-final-paragraph t)
						   (:html . nil))
				     :stream nil)))
	     "")))))

#|
(defvar *x*)

(defextension (property :arguments ((name :required)))
  (ecase phase
    (:parse)
    (:render
     (bind (((:values d s)
	     (markdown (document-property name) 
		       :parent *current-document*
		       :format *current-format*
		       :properties '((:omit-initial-paragraph t)
				     (:omit-final-paragraph t)
				     (:html . nil))
		       :stream nil)))
       (setf *x* d)
       (prog1
	   (strip-whitespace s))))))

(let ((*current-document* *x*))
  (document-property "html"))

(form-property-name "html")

(trace item-at-1)

|#


#+(or)
;;??
(defextension (set-property :arguments ((name :required) 
					(value :whole)))
  (when (eq phase :parse)
    (setf (document-property name) value))
  nil)

(defun set-property (phase args result)
  (declare (ignorable phase args result))
  (bind ((name (pop args))
	 (value
	  (progn (if (length-1-list-p args)
		     (first args)
		     args))))
    (assert name nil "name is required")
    (when (eq phase :parse)
      (setf (document-property name) value))
    nil))

(defextension (table-of-contents :arguments ((depth :required :keyword)
					     (start :required :keyword)
					     (label :keyword)))
  (ecase phase 
    (:parse
     (push (lambda (document)
	     (add-anchors document :depth depth :start start))
	   (item-at-1 (properties *current-document*) :cleanup-functions))
     nil) 
    (:render
     (bind ((headers (collect-elements
		      (chunks *current-document*)
		      :filter (lambda (x) 
				(header-p x :depth depth :start start)))))
       (when headers
	 (format *output-stream* 
		 "~&<a name='table-of-contents' id='table-of-contents'></a>")
	 (format *output-stream* "~&<div class='table-of-contents'>~%")
	 (when label
	   (format *output-stream* "<h1>~a</h1>" label))
	 (iterate-elements 
	  headers
	  (lambda (header)
	    (bind (((index level text)
		    (item-at-1 (properties header) :anchor))
		   (save-header-lines (copy-list (lines header))))
	      #+(or)
	      (format *output-stream* 
		      "~&<a href='#~a' title='~a'>"
		      (make-ref index level text)
		      (or text ""))
	      (setf (slot-value header 'lines)
		    `(,(format nil
			       "~&<a href='#~a' title='~a'>"
			       (make-ref index level text)
			       (or text ""))
		       ,@(lines header)
		       ,(format nil "</a>")))
	      (render-to-html header nil)
	      (setf (slot-value header 'lines)
		    save-header-lines)
	      #+(or)
	      (format *output-stream* "</a>"))))
	 (format *output-stream* "~&</div>~%"))))))

(defsimple-extension toc-link
  (format nil "~&<a href='#table-of-contents'>Top</a>"))

(defun make-ref (index level text)
  (declare (ignore text))
  (format nil "~(~a-~a~)" level index))

(defun add-anchors (document &key depth start)
  (let* ((index -1)
         (header-level nil)
         (header-indexes (nreverse
                          (collect-elements
                           (chunks document)
                           :transform
                           (lambda (chunk) 
                             (setf (item-at-1 (properties chunk) :anchor)
                                   (list index header-level
                                         (first-item (lines chunk)))))
                           :filter 
                           (lambda (chunk)
                             (incf index) 
                             (setf header-level
                                   (header-p chunk :depth depth 
					     :start start)))))))
    (iterate-elements 
     header-indexes
     (lambda (datum)
       (bind (((index level text) datum)
              (ref (make-ref index level text)))
         (anchor :parse `(,ref ,text) nil)
         (insert-item-at 
          (chunks document)
          (make-instance 'chunk 
            :lines `((eval anchor (,ref nil) nil t)))
          index))))))
    
(defun header-p (chunk &key depth start)
  (let* ((header-elements  '(header1 header2 header3 
                             header4 header5 header6))
         (header-elements (subseq header-elements
                                  (1- (or start 1))
                                  (min (or depth (length header-elements))
                                         (length header-elements)))))
    (some-element-p (markup-class chunk)
                    (lambda (class)
                      (member class header-elements)))))

#+(or)
(markdown "{set-property html t}
html = {property html}
 I like {docs markdown function}, don't you." :additional-extensions '(docs))

#+(or)
(markdown 
 "{set-property docs-package asdf-install}
{set-property 
{docs install function}
{docs asdf-install:*gnu-tar-program* variable}
"
 :additional-extensions '(docs))

#|
{set-property docs-package asdf-install}
{set-property docs-heading-level 4}
{set-property docs-heading-format "%type %name:"}
{docs *gnu-tar-program* variable}
|#

(defextension (abbrev :arguments ((abbreviation :required)
				  (text :required :whole)))
  (ecase phase
    (:parse
     ;; no worries
     )
    (:render 
     (format nil "~a" abbreviation))))

(defextension (include :arguments ((pathname :required)))
  (ecase phase
    (:parse
     ;; no worries
     (setf pathname (find-include-file pathname))
     )
    (:render
     (process-child-markdown 
      (first result) :transfer-data t))))

(defextension (include-if :arguments ((test :required) (pathname :required)))
  (ecase phase
    (:parse
     ;; no worries
     (setf pathname (find-include-file pathname))
     )
    (:render
     (when (document-property test)
       (process-child-markdown 
	(first result) :transfer-data t)))))

(defun find-include-file (pathname)
  (bind ((pathname (ensure-string pathname))
	 (search-locations (ensure-list (document-property :search-locations)))
	 (result
	  (or (probe-file (merge-pathnames pathname))
	      ;; look in search-locations
	      (some (lambda (location)
		      (probe-file (merge-pathnames pathname location)))
		    search-locations))))
    (unless result
      (markdown-warning 
       "Unable to find ~a in any of the search-locations ~{~a~^, ~}"
       pathname search-locations))
    result))
  
(defun process-child-markdown (text &key (transfer-data nil))
  (bind (((:values child output)
	  (markdown text 
		    :parent *current-document*
		    :format *current-format*
		    :properties '((:omit-initial-paragraph t)
				  (:omit-final-paragraph t)
				  (:html . nil))
		    :stream nil)))
    (when transfer-data
      (transfer-link-info *current-document* child "")
      (transfer-selected-properties 
       *current-document* child
       (set-difference (collect-keys (properties child))
		       (list :style-sheet :style-sheets :title))))
    (strip-whitespace output)))
