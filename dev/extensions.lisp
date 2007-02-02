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

(defun today (phase arguments result)
  (declare (ignore phase arguments result))
  (let ((format (document-property :date-format "%e %B %Y")))
    (format-date format (get-universal-time))))

(defun now (phase arguments result)
  (declare (ignore phase arguments result))
  (let ((format (document-property :time-format "%H:%M")))
    (format *output-stream* "~a" (format-date format (get-universal-time)))
    nil))

(defun table-of-contents (phase args result)
  (declare (ignore result))
  (bind ((args (mapcar (lambda (x) (ignore-errors (read-from-string x))) args))
         (depth (getf args :depth))
         (start (getf args :start-at)))
    ;(spy args depth start)
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
           (format *output-stream* "<div class='table-of-contents'>")
           (iterate-elements headers
                             (lambda (header)
                               (bind (((index level text)
                                       (item-at-1 (properties header) :anchor)))
                                 (format *output-stream* "<a href='#~a' title='~a'>"
                                         (make-ref index level text)
                                         (or text ""))
                                 (render-to-html header)
                                 (format *output-stream* "</a>"))))
           (format *output-stream* "</div>")))))))

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
                                   (header-p chunk :depth depth :start start)))))))
    (iterate-elements 
     header-indexes
     (lambda (datum)
       (bind (((index level text) datum)
              (ref (make-ref index level text)))
         (anchor :parse `(,ref ,text))
         (insert-item-at 
          (chunks document)
          (make-instance 'chunk 
            :lines `((eval anchor (,ref) nil t)))
          index))))))
    
(defun header-p (chunk &key depth start)
  (let* ((header-elements  '(header1 header2 header3 
                             header4 header5 header6))
         (header-elements (subseq header-elements
                                  (or (1- start) 0)
                                  (min (or depth (length header-elements))
                                         (length header-elements)))))
    (some-element-p (markup-class chunk)
                    (lambda (class)
                      (member class header-elements)))))

(defun anchor (phase &rest args)
  (ecase phase
    (:parse
     (let ((name (caar args))
           (title (cadar args)))
       (setf (item-at (link-info *current-document*) name)
             (make-instance 'link-info
               :id name :url (format nil "#~a" name) :title (or title "")))))
    (:render (let ((name (caar args)))
               (format nil "<a name='~a' id='~a'></a>" name name)))))

(defun property (phase args result)
  (declare (ignore result phase))
  (if (length-at-least-p args 1)
    (bind (((name &rest args) args))
      (when args
        (warn "Extra arguments to property"))
      (document-property name))
    (warn "Not enough arguments to property (need at least 1)")))

(defun set-property (phase args result)
  ;; {set-property name value}
  (declare (ignore result))
  (when (eq phase :parse)
    (if (length-at-least-p args 2)
      (bind (((name &rest value) args))
        (setf value (format nil "~{~a~^ ~}" value))
        (setf (document-property name) value))
      (warn "Not enough arguments to set-property (need at least 2)")))
  nil)

(defun docs-package ()
  (let ((property (document-property :docs-package)))
    (or (find-package property) 
	(find-package (string-upcase property)))))

(defun docs (phase args result)
  ;; {documentation thing &optional type}
  (declare (ignore result))
  (when (eq phase :render)
    ;;?? could memoize this
    (bind (((thing &optional type) args)
	   (thing (let ((*package* (or (docs-package) *package*)))
		    (with-input-from-string (in thing) (read in))))
	   (type (or (and type 
			  (with-input-from-string (in type) (read in)))
		     (loop for type in '(function variable package setf
					      type structure compiler-macro
					      method-combination t)
			       when (documentation thing type) do
			       (return type))))
	   (docs (documentation thing type)))
      (when docs
	(format *output-stream* "~&<div class=\"documentation ~(~a~)\">" type)
	(markdown docs
		  :stream *output-stream*
		  :format *current-format*)
	(format *output-stream* "~&</div>"))
      nil)))

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
