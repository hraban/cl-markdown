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


(defsimple-extension current-year
  (let ((format (document-property :date-format "%Y")))
    (format-date format (get-universal-time))))

(defsimple-extension today 
  (let ((format (document-property :date-format "%e %B %Y")))
    (format-date format (get-universal-time))))

(defsimple-extension now 
  (let ((format (document-property :time-format "%H:%M")))
    (format-date format (get-universal-time))))

(defextension (comment :arguments ((text :required))
		       :insertp t)
  (ecase phase
    (:parse
     ;; no worries
     )
    (:render 
     (format nil "<!-- ~a -->" text))))

(defextension (remark :arguments ((text :required)) 
		      :insertp t)
  (ecase phase
    (:parse
     ;; no worries
     )
    (:render 
     ;; stil no worries
     )))

(defextension (anchor :arguments ((name :required) title) :insertp t)
  (setf name (ensure-string name))
  (let ((safe-name (html-safe-name name)))
    (ecase phase
      (:parse
       (setf (item-at (link-info *current-document*) name)
	     (make-instance 'link-info
			    :id name
			    :url (format nil "#~a" safe-name) 
			    :title (or title ""))))
      (:render 
       (format nil "<a name='~a' id='~a'></a>" safe-name safe-name)))))

(defextension (property :arguments ((name :required))
			:insertp t)
  (ecase phase
    (:parse)
    (:render
     (process-child-markdown (document-property name) phase))))

(defextension (ifdef :arguments ((keys :required) 
				 (text :required :whole))
		     :insertp t)
  (ecase phase
    (:parse)
    (:render
     (prog1
	 (if (or (and (atom keys) (document-property keys))
		 )
	     (process-child-markdown 
	      (format nil "~{~a~^ ~}" (ensure-list text)) phase)
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


(defextension (set-property :arguments ((name :required) 
					(value :whole))
			    :insertp t)
  (when (eq phase :parse)
    (setf (document-property name) value))
  nil)

#+(or)
;;??
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
					     (label :keyword))
				  :insertp t)
  (ecase phase 
    (:parse
     (push (lambda (document)
	     (add-toc-anchors document :depth depth :start start))
	   (item-at-1 (properties *current-document*) :cleanup-functions))
     nil) 
    (:render
     (bind ((headers (collect-toc-headings depth start)))
       (when headers
	 (format *output-stream* 
		 "~&<a name='table-of-contents' id='table-of-contents'></a>")
	 (format *output-stream* "~&<div class='table-of-contents'>~%")
	 (when label
	   (format *output-stream* "<h1>~a</h1>" label))
	 (iterate-elements 
	  headers
	  (lambda (header)
	    (bind (((nil anchor text)
		    (item-at-1 (properties header) :anchor))
		   (save-header-lines (copy-list (lines header))))
	      (setf (slot-value header 'lines)
		    `(,(format nil
			       "~&<a href='~a~a' ~@[title='~a'~]>"
			       (if (char= (aref anchor 0) #\#) "" "#")
			       anchor
			       (encode-string-for-title text))
		       ,@(lines header)
		       ,(format nil "</a>")))
	      (render-to-html header nil)
	      (setf (slot-value header 'lines)
		    save-header-lines))))
	 (format *output-stream* "~&</div>~%"))))))

(defun collect-toc-headings (depth start)
  (collect-elements
   (chunks *current-document*)
   :filter (lambda (x) 
	     (header-p x :depth depth :start start))))

(defsimple-extension toc-link
  (format nil "~&<a href='#table-of-contents'>Top</a>"))

(defun make-ref (index level)
  (format nil "~(~a-~a~)" level index))

(defun add-toc-anchors (document &key depth start)
  (let* ((index -1)
         (header-level nil)
	 (last-anchor nil)
         (header-indexes
	  (nreverse
	   (collect-elements
	    (chunks document)
	    :transform
	    (lambda (chunk) 
	      (item-at-1 (properties chunk) :anchor))
	    :filter 
	    (lambda (chunk)
	      (incf index) 
	      (let ((it nil))
		(cond ((setf it (header-p chunk :depth depth 
					  :start start))
		       (setf header-level it)
		       (setf (item-at-1 (properties chunk) :anchor)
			     (list index
				   (or (and last-anchor
					    (url last-anchor))
				       (make-ref index header-level))
				   (with-output (*output-stream* nil) 
				     (render-plain chunk))))
		       (null last-anchor))
		     ((setf it (simple-anchor-p chunk))
		      (setf last-anchor it)
		      nil)
		     (t 
		      (setf last-anchor nil)))))))))
    (iterate-elements 
     header-indexes
     (lambda (datum)
;       (print datum)
       (bind (((index ref text) datum))
	 (anchor :parse `(,ref ,text) nil)
	 (insert-item-at 
	  (chunks document)
	  (make-instance 'chunk 
			 :lines `((eval anchor (,ref nil) nil t)))
	  index))))))
  
(defun simple-anchor-p (chunk)
  (or (and (plusp (size (lines chunk)))
	   (let ((link-name nil) (title nil))
	     (when (some-element-p 
		    (lines chunk)
		    (lambda (line)
		      (when (consp line)
			(case (first line)
			  (simple-anchor 
			   (setf link-name (second line)))
			  (anchor-with-text
			   (setf link-name (third line) title (second line)))))))
	       (make-instance 'link-info
			      :id link-name
			      :url (format nil "#~a" (html-safe-name link-name)) 
			      :title (or title "")))))
      (and (< 0 (size (lines chunk)) 3)
	   (length-at-least-p (first-element (lines chunk)) 2)
	   (equal (subseq (first-element (lines chunk)) 0 2)
		  '(eval anchor))
	   (fourth (first-element (lines chunk))))))

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

(defextension (include :arguments ((pathname :required))
		       :insertp t)
  (ecase phase
    (:parse
     ;; no worries
     (let ((pathname (find-include-file pathname)))
       ;; FIXME - if I use a list, someone in markdown calls chunks on it.
       (make-array 1 :initial-contents 
		   (list (process-child-markdown 
			  pathname phase :transfer-data t)))))
    (:render
     (when result
       (render-to-stream (aref (first result) 0) *current-format* nil)))))

(defextension (include-if :arguments ((test :required) (pathname :required))
			  :insertp t)
  (ecase phase
    (:parse
     (when (document-property test)
       (let ((pathname (find-include-file pathname)))
	 ;; FIXME - if I use a list, someone in markdown calls chunks on it.
	 (make-array 1 :initial-contents 
		     (list (process-child-markdown 
			    pathname phase :transfer-data t))))))
    (:render
     (when result
       (render-to-stream (aref (first result) 0) *current-format* nil)))))
