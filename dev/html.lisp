(in-package #:cl-markdown)

(defstruct (html-markup
	     (:conc-name markup-)
	     (:print-object print-html-markup))
  name outer inner tag encoding-method contentlessp (nestsp t))

(defmethod print-html-markup (markup stream)
  (print-unreadable-object (markup stream :type nil :identity t)
    (bind (((:struct markup- name inner outer encoding-method) markup))
      (format stream "~a : ~a ~a ~a" 
	      name outer inner encoding-method))))

(defparameter *markup->html*
  (make-container 
   'simple-associative-container
   :test #'equal
   :initial-contents 
   (loop for datum in '((header1 nil nil "h1")
			(header2 nil nil "h2")
			(header3 nil nil "h3")
			(header4 nil nil "h4")
			(header5 nil nil "h5")
			(header6 nil nil "h6")     
			(bullet  ("ul") ("li") nil)
			(code    ("pre" "code") nil nil encode-pre nil nil)
			(number  ("ol") ("li") nil)
			(quote ("blockquote") nil nil)
			(horizontal-rule nil nil "hr" nil t)) nconc
	(bind (((tag outer inner &optional markup new-method 
		     contentlessp (nestsp t))
		datum))
	  (list (list tag)
		(make-html-markup
		       :name tag
		       :outer outer
		       :inner inner
		       :tag markup
		       :encoding-method new-method
		       :contentlessp contentlessp
		       :nestsp nestsp))))))

(defvar *magic-space-p* nil)

(defvar *magic-line-p* 0)

(defparameter *magic-space* #\Space)

(defparameter *magic-line* nil)

#+(or)
(setf *magic-space* #\| *magic-line* #\~)

(defgeneric render-to-html (stuff encoding-method)
  (:documentation ""))

(defmethod render ((document abstract-document) (style (eql :html)) stream)
  (declare (ignore stream))
  (setf *magic-space-p* nil 
	*magic-line-p* 0)
  (render-to-html document nil))

(defun html-block-markup (chunk)
  (aand 
   (markup-class-for-html chunk)
   (markup-outer it)))

(defun html-inner-block-markup (chunk)
  (aand
   (markup-class-for-html chunk)
   (markup-inner it)))

(defmethod render-to-html ((chunk chunk) encoding-method)
  (bind (((:struct markup- (markup tag) 
		   (new-method encoding-method) contentlessp)
	  (markup-class-for-html chunk))
         (paragraph? (paragraph? chunk)))
    (cond (contentlessp
	   (format *output-stream* "<~a/>" markup))
	  (t
	   (encode-html chunk (or new-method encoding-method)
			markup (when paragraph? "p"))))))

;;?? same code as below
(defmethod encode-html ((stuff chunk) encoding-method &rest codes)
  (declare (dynamic-extent codes))
  (setf *magic-line-p* 0)
  (cond ((null codes) 
         (let ((code-p (member 'code (markup-class stuff))))
           (iterate-elements
            (lines stuff)
            (lambda (line)
	      (render-to-html line encoding-method)
	      (when code-p (incf *magic-line-p*))))
	   (fresh-line *output-stream*)))
        ((null (first codes))
         (apply #'encode-html stuff encoding-method (rest codes)))
        (t (format *output-stream* "<~a>" (first codes))
           (apply #'encode-html stuff encoding-method (rest codes))
           (format *output-stream* "</~a>" (first codes))
	   #+(or)
           (unless (length-1-list-p codes) 
             (terpri *output-stream*)))))

;;?? same code as above
(defmethod encode-html ((stuff list) encoding-method &rest codes)
  (declare (dynamic-extent codes))
  (cond ((null codes) 
         (iterate-elements
          stuff
          (lambda (line)
            (render-to-html line encoding-method))))
        ((null (first codes))
         (apply #'encode-html stuff encoding-method (rest codes)))
        (t (format *output-stream* "<~A>" (first codes))
           (apply #'encode-html stuff encoding-method (rest codes))
           (format *output-stream* "</~A>" (first codes))
	   #+(or)
           (unless (length-1-list-p codes) 
             (terpri *output-stream*)))))

(defmethod markup-class-for-html ((chunk chunk))
  (if (markup-class chunk)
    (let ((translation (item-at-1 *markup->html* (markup-class chunk))))
      (unless translation 
        (markdown-warning "No translation for markup class '~A'"
			  (markup-class chunk)))
      translation)
    (load-time-value (make-html-markup))))

(defmethod render-to-html ((chunk list) encoding-method)
  (render-span-to-html (first chunk) (rest chunk) encoding-method))

(defmethod render-to-html ((line string) encoding-method)
  (when *magic-space-p* 
    (setf *magic-space-p* nil)
    (princ *magic-space* *output-stream*))
  (when (> *magic-line-p* 0)
    (when *magic-line*
      (princ *magic-line* *output-stream*))
    (terpri *output-stream*))
  (format *output-stream* "~a"  
	  (funcall (or encoding-method 'encode-string-for-html) line))
  (setf *magic-space-p* t))

(defun output-html (string &rest codes)
  (declare (dynamic-extent codes))
  (cond ((null codes) (princ (first string) *output-stream*))
        (t (format *output-stream* "<~(~a~)>" (first codes))
           (apply #'output-html string (rest codes))
           (format *output-stream* "</~(~a~)>" (first codes)))))

(defmethod render-span-to-html ((code (eql 'strong)) body encoding-method)
  (declare (ignore encoding-method))
  (output-html body 'strong)
  (setf *magic-space-p* nil))

(defmethod render-span-to-html ((code (eql 'mail)) body encoding-method)
  (declare (ignore encoding-method))
  (let ((address (first body)))
    (output-link (format nil "mailto:~A" address) nil address)))

(defmethod render-span-to-html ((code (eql 'emphasis)) body encoding-method)
  (declare (ignore encoding-method))
  (output-html body 'em)
  (setf *magic-space-p* nil))

(defmethod render-span-to-html ((code (eql 'strong-em)) body encoding-method)
  (declare (ignore encoding-method))
  (output-html body 'strong 'em)
  (setf *magic-space-p* nil))

(defmethod render-span-to-html 
    ((code (eql 'escaped-character)) body encoding-method)
  (declare (ignore encoding-method))
  (let ((char (aref (first body) 0)))
    (cond ((char= #\< char) (princ "&lt;" *output-stream*))
	  ((char= #\> char) (princ "&gt;" *output-stream*))
	  (t (output-html body))))
  (setf *magic-space-p* nil))

(defmethod render-span-to-html ((code (eql 'code)) body encoding-method)
  (format *output-stream* "<code>")
  (setf *magic-space-p* nil)
  (dolist (bit body)
    (render-to-html bit encoding-method))
  (format *output-stream* "</code>")
  (setf *magic-space-p* nil))

(defmethod render-span-to-html ((code (eql 'entity)) body encoding-method)
  (declare (ignore encoding-method))
  (setf *magic-space-p* nil
	*magic-line-p* -1)
  (output-html body)
  (setf *magic-space-p* nil
	*magic-line-p* -1))

(defmethod render-span-to-html 
    ((code (eql 'reference-link)) body encoding-method)
  (declare (ignore encoding-method))
  (bind (((:values text id nil)
          (if (length-1-list-p body)
            (values (first body) (first body) nil)
            (values (butlast body 1) (first (last body)) t)))
         (link-info (find-link id)))
    (cond ((not (null link-info))
           ;; it _was_ a valid ID
	   (generate-link-output link-info text))
          (t
	   (markdown-warning "No reference found for link ~s" id)
	   (format *output-stream* "~a" (if (consp text) (first text) text))
	   (setf *magic-space-p* nil)))))

(defmethod generate-link-output ((link-info link-info) text)
  (output-link (url link-info) (title link-info) text (properties link-info)))

(defmethod generate-link-output ((link-info extended-link-info) text)
  ;; you didn't really want it to be fast did you...?
  (generate-link-output-for-kind (kind link-info) link-info text))

(defmethod render-span-to-html 
    ((code (eql 'inline-link)) body encoding-method)
  (declare (ignore encoding-method))
  (bind (((text &optional (url "") title) body))
    (output-link url title text)))

(defmethod render-span-to-html ((code (eql 'link)) body encoding-method)
  (declare (ignore encoding-method))
  (let ((link (first body)))
    (output-link link nil link)))

(defmethod render-span-to-html 
    ((code (eql 'reference-image)) body encoding-method)
  (declare (ignore encoding-method))
  (bind (((:values text id nil)
          (if (length-1-list-p body)
            (values (first body) (first body) nil)
            (values (butlast body 1) (first (last body)) t)))
         (link-info (find-link id)))
    (cond ((not (null link-info))
           ;; it _was_ a valid ID
           (output-image (url link-info) (title link-info) text
			 (properties link-info)))
          (t
           ;;?? hackish
           (format *output-stream* "[~a][~a]" text (if supplied? id ""))
	   (setf *magic-space-p* nil)))))

(defmethod render-span-to-html 
    ((code (eql 'inline-image)) body encoding-method)
  (declare (ignore encoding-method))
  (bind (((text &optional (url "") title) body))
    (output-image url title text)))

(defun output-link (url title text &optional properties)
  (cond ((not (null url))
         (format *output-stream* "<a href=\"~A\"~@[ title=\"~A\"~]"
                 url title)
	 (loop for (key . value) in properties do 
	      (format *output-stream* " ~a=\"~a\"" key value))
	 (write-string ">" *output-stream*)
	 (setf *magic-space-p* nil)
         (encode-html (ensure-list text) nil)
         (format *output-stream* "</a>")
	 (setf *magic-space-p* nil))
        (t
         )))

(defun output-image (url title text &optional properties)
  (cond ((not (null url))
         (format *output-stream*
		 "<img src=\"~A\"~@[ title=\"~A\"~]~@[ alt=\"~A\"~]"
                 url title (first (ensure-list text)))
	 (loop for (key . value) in properties do 
	      (format *output-stream* " ~a=\"~a\"" key value))
	 (write-string "></img>" *output-stream*)
	 (setf *magic-space-p* nil))
        (t
         )))

(defmethod render-span-to-html ((code (eql 'html)) body encoding-method)
  ;; hack!
  (let ((output (first body)))
    (etypecase output
      (string 
       (output-html (list (encode-pre output))))
      (list
       (render-span-to-html (first output) (rest output) encoding-method)))))

;; Special cases R us.
(defmethod render-span-to-html ((code (eql 'break)) body encoding-method)
  (encode-html body encoding-method)
  (format *output-stream* "<br />~%"))

(defun stream-out-markup (markup reverse)
  (dolist (marker (if reverse (reverse markup) markup))
    (let ((cr? (member marker
		       *block-level-html-tags* :test 'string=)))
      (when (and (not reverse) cr?)
	(terpri *output-stream*))
      (format *output-stream* "<~a~a>" 
	      (if reverse "/" "") marker))))

;; FIXME - in case you're wondering, this is an ugly bit of code
(defmethod render-to-html ((document abstract-document) encoding-method) 
  (bind ((current-chunk nil)
	 (wrap-in-html (add-html-header-p document)))
    (labels ((render-block (block level markup inner?)
;	       (print (list :rb level inner? (first block))) 
	       (setf *magic-space-p* nil)
	       (let ((add-markup? (not (eq (first block) current-chunk)))
		     (real-markup 
		      (if (and (not inner?) (length-1-list-p block))
			  (append
			   markup (html-inner-block-markup (first block)))
			  markup)))
		 (when add-markup?
		   (stream-out-markup real-markup nil))
		 (cond ((or (length-1-list-p block)
			    )
			(render-to-html (first block) encoding-method))
		       ((not add-markup?) 
			(render-to-html (first block) encoding-method)
			(do-it (rest block) level))
		       (t
			(setf current-chunk (and inner? (first block)))
			(do-it block level)))
		 (when add-markup?
		   (stream-out-markup real-markup t))))
	     (do-it (chunks level)
;	       (print (list :di level (first chunks))) 
	       (loop for rest = chunks then (rest rest) 
		  for chunk = (first rest) then (first rest) 
		  while chunk 
		  for new-level = (level chunk)
		  when (= level new-level) do
		  (let ((index (inner-block rest))
			(inner-markup (html-inner-block-markup chunk)))
		    (render-block (subseq rest 0 index)
				  level inner-markup t)
		    (setf rest (nthcdr (1- index) rest)))
		  when (< level new-level) do
		  (multiple-value-bind (block remaining method)
		      (next-block rest new-level)
		    (declare (ignore method))
		    (render-block 
		     block new-level (html-block-markup chunk) nil)
		    (setf rest remaining)))))
      (when wrap-in-html
	(generate-html-header))
      (do-it (collect-elements (chunks document)) (level document))
      (when wrap-in-html
	(format *output-stream* "~&</body>~&</html>~%")))))

(defmethod add-html-header-p ((document abstract-document))
  (values nil))

(defmethod add-html-header-p ((document document))
  (document-property :html))

(defun inner-block (chunks)
;  (print (list :ib (first chunks))) 
  (let* ((level (level (first chunks)))
	 (markup-class (markup-class (first chunks))))
    (or (aand (position-if
	       (lambda (chunk)
		 (or (< (level chunk) level)
		     (and (= (level chunk) level)
			  (not (equal (markup-class chunk) markup-class)))))
	       (rest chunks))
	      (1+ it))
	(length chunks))))

(defvar *html-meta*
  '((name (author description copyright keywords date))
    (http-equiv (refresh expires))))

(defun generate-html-header ()
  (generate-doctype)
  (format *output-stream* "~&<html xmlns='~a' xml:lang='~a' lang='~a'>"
	  (document-property :xmlns "http://www.w3.org/1999/xhtml")
	  (document-property :xmllang "en")
	  (document-property :lang "en"))
  (format *output-stream* "~&<head>")
  (awhen (document-property "title")
    (format *output-stream* "~&<title>~a</title>" 
	    (process-child-markdown it :transfer-data nil)))
  (let ((styles nil))
    (flet ((output-style (it)
	     (bind (((name &optional media) (ensure-list it)))
	       (setf name (ensure-string name))
	       (unless (search ".css" name)
		 (setf name (concatenate 'string name ".css")))
	       (unless (member name styles :test 'string-equal)
		 (push name styles)
		 (if (document-property :make-style-sheet-inline)
		     (insert-style-sheet name media)
		     (format 
		      *output-stream* 
		      "~&<link type='text/css' href='~a' rel='stylesheet' ~@[media='~a' ~]/>"
		  name media))))))
      (awhen (document-property "style-sheet")
	(output-style it))
      (loop for style in (document-property "style-sheets") do
	   (output-style style))
      (loop for (kind properties) in *html-meta* do
	   (loop for property in properties do
		(awhen (document-property (symbol-name property))
		  (format *output-stream* "~&<meta ~a=\"~a\" content=\"~a\"/>"
			  kind property it))))
      (format *output-stream* "~&</head>~&<body>"))))

(defun generate-doctype ()
  (format *output-stream* "~&<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
        \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"))

(defun insert-style-sheet (name media)
  (let ((pathname (find-include-file name)))
    (when pathname
      (format *output-stream* "~&<style type='text/css'~@[ media='~a'~]>~%"
	      media)
      (map-lines (pathname pathname) 
		 (lambda (line)
		   (format *output-stream* "~&  ~a~%" line)))
      (format *output-stream* "~&</style>~%"))))

(defun output-anchor (name &optional (stream *output-stream*))
  (let ((name (html-safe-name (ensure-string name))))
    (format stream
	    "~&<a name=\"~a\" id=\"~a\"></a>~%" 
	    name name)))

