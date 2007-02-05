(in-package #:cl-markdown)

(declaim (optimize (speed 0) (space 0) (debug 3)))

;;; ---------------------------------------------------------------------------

(defparameter *markup->html*
  (make-container 
   'simple-associative-container
   :test #'equal
   :initial-contents 
   '((header1)    (nil "h1")
     (header2)    (nil "h2")
     (header3)    (nil "h3")
     (header4)    (nil "h4")
     (header5)    (nil "h5")
     (header6)    (nil "h6")
     
     (bullet)     (("ul") "li")
     (code)       (("pre" "code") nil)
     (number)     (("ol") "li")
     (quote)      (("blockquote") nil)
     (horizontal-rule) (nil "hr"))))

;;; ---------------------------------------------------------------------------

(defmethod render ((document document) (style (eql :html)) stream)
  (render-to-html document))

;;; ---------------------------------------------------------------------------

(defgeneric render-to-html (stuff)
  (:documentation ""))

;;; ---------------------------------------------------------------------------

(defun html-marker (chunk)
  (bind ((markup (markup-class-for-html chunk)))
    (first markup)))

;;; ---------------------------------------------------------------------------

(defmethod render-to-html ((chunk chunk))
  (bind ((markup (second (markup-class-for-html chunk)))
         (paragraph? (paragraph? chunk)))
    (encode-html chunk markup (when paragraph? "p"))))
  
;;; ---------------------------------------------------------------------------
  
(defmethod encode-html ((stuff chunk) &rest codes)
  (declare (dynamic-extent codes))
  (cond ((null codes) 
         (let ((class (member 'code (markup-class stuff))))
           (iterate-elements
            (lines stuff)
            (lambda (line)
              (render-to-html line)
              (when class (format *output-stream* "~%"))))))
        ((null (first codes))
         (apply #'encode-html stuff (rest codes)))
        (t (format *output-stream* "<~a>" (first codes))
           (apply #'encode-html stuff (rest codes))
           (format *output-stream* "</~a>" (first codes))
           (unless (length-1-list-p codes) 
             (terpri *output-stream*)))))

;;; ---------------------------------------------------------------------------

(defmethod encode-html ((stuff list) &rest codes)
  (declare (dynamic-extent codes))
  (cond ((null codes) 
         (iterate-elements
          stuff
          (lambda (line)
            ;(spy (type-of line))
            (render-to-html line))))
        ((null (first codes))
         (apply #'encode-html stuff (rest codes)))
        (t (format *output-stream* "<~A>" (first codes))
           (apply #'encode-html stuff (rest codes))
           (format *output-stream* "</~A>" (first codes))
           (unless (length-1-list-p codes) 
             (terpri *output-stream*)))))

;;; ---------------------------------------------------------------------------

(defmethod markup-class-for-html ((chunk chunk))
  (when (markup-class chunk)
    (let ((translation (item-at-1 *markup->html* (markup-class chunk))))
      (unless translation 
        (warn "No translation for '~A'" (markup-class chunk)))
      translation)))

;;; ---------------------------------------------------------------------------

(defmethod render-to-html ((chunk list))
  (render-span-to-html (first chunk) (rest chunk)))

;;; ---------------------------------------------------------------------------

(defmethod render-to-html ((line string))
  (format *output-stream* "~A" line #+(or) (html-encode:encode-for-pre line)))

;;; ---------------------------------------------------------------------------

(defun output-html (string &rest codes)
  (declare (dynamic-extent codes))
  (cond ((null codes) (princ (first string) *output-stream*))
        (t (format *output-stream* "<~A>" (first codes))
           (apply #'output-html string (rest codes))
           (format *output-stream* "</~A>" (first codes)))))

;;; ---------------------------------------------------------------------------

(defmethod render-span-to-html ((code (eql 'strong)) body)
  (output-html body 'strong))

;;; ---------------------------------------------------------------------------

(defmethod render-span-to-html ((code (eql 'mail)) body)
  (let ((address (first body)))
    (output-link (format nil "mailto:~A" address) nil address)))

;;; ---------------------------------------------------------------------------

(defmethod render-span-to-html ((code (eql 'emphasis)) body)
  (output-html body 'em))

;;; ---------------------------------------------------------------------------

(defmethod render-span-to-html ((code (eql 'strong-em)) body)
  (output-html body 'strong 'em))

;;; ---------------------------------------------------------------------------

(defmethod render-span-to-html ((code (eql 'escaped-character)) body)
  (output-html body))

;;; ---------------------------------------------------------------------------

(defmethod render-span-to-html ((code (eql 'code)) body)
  (format *output-stream* "<code>")
  (render-to-html (first body))
  (format *output-stream* "</code>"))

;;; ---------------------------------------------------------------------------

(defmethod render-span-to-html ((code (eql 'entity)) body)
  (output-html body))

;;; ---------------------------------------------------------------------------

(defmethod render-span-to-html ((code (eql 'reference-link)) body)
  (bind (((values text id supplied?)
          (if (length-1-list-p body)
            (values (first body) (first body) nil)
            (values (butlast body 1) (first (last body)) t)))
         (link-info (item-at-1 (link-info *current-document*) id)))
    (cond ((not (null link-info))
           ;; it _was_ a valid ID
           (output-link (url link-info) (title link-info) text))
          (t
           ;;?? hackish
           (format *output-stream* "[~a][~a]" text (if supplied? id ""))))))

;;; ---------------------------------------------------------------------------

(defmethod render-span-to-html ((code (eql 'inline-link)) body)
  (bind (((text &optional (url "") title) body))
    (output-link url title text)))

;;; ---------------------------------------------------------------------------

(defmethod render-span-to-html ((code (eql 'link)) body)
  (let ((link (first body)))
    (output-link link nil link)))

;;; ---------------------------------------------------------------------------

(defmethod render-span-to-html ((code (eql 'reference-image)) body)
  (bind (((values text id supplied?)
          (if (length-1-list-p body)
            (values (first body) (first body) nil)
            (values (butlast body 1) (first (last body)) t)))
         (link-info (item-at-1 (link-info *current-document*) id)))
    (cond ((not (null link-info))
           ;; it _was_ a valid ID
           (output-image (url link-info) (title link-info) text))
          (t
           ;;?? hackish
           (format *output-stream* "[~a][~a]" text (if supplied? id ""))))))

;;; ---------------------------------------------------------------------------

(defmethod render-span-to-html ((code (eql 'inline-image)) body)
  (bind (((text &optional (url "") title) body))
    (output-image url title text)))

;;; ---------------------------------------------------------------------------

(defun output-link (url title text)
  (cond ((not (null url))
         (format *output-stream* "<a href=\"~A\"~@[ title=\"~A\"~]>"
                 url title)
         (encode-html (ensure-list text))
         ;(render-span-to-html 'html (list text))
         (format *output-stream* "</a>"))
        (t
         )))

;;; ---------------------------------------------------------------------------

(defun output-image (url title text)
  (cond ((not (null url))
         (format *output-stream* "<img src=\"~A\"~@[ title=\"~A\"~]~@[ alt=\"~A\"~]></img>"
                 url title text))
        (t
         )))

;;; ---------------------------------------------------------------------------

(defmethod render-span-to-html ((code (eql 'html)) body)
  ;; hack!
  (let ((output (first body)))
    (etypecase output
      (string 
       (output-html (list (html-encode:encode-for-pre output))))
      (list
       (render-span-to-html (first output) (rest output))))))

;;; ---------------------------------------------------------------------------

(defmethod render-to-html ((document document)) 
  (labels ((do-it (chunks level)
             (loop for rest = chunks then (rest rest) 
                   for chunk = (first rest) then (first rest) 
                   while chunk 
                   for new-level = (level chunk)
                   for markup = (html-marker chunk) 
                   when (= level new-level) do (render-to-html chunk)
                   when (< level new-level) do
                   (dolist (marker markup)
                     (format *output-stream* "<~A>" marker))
                   (multiple-value-bind (block remaining method)
                                        (next-block rest new-level)
                     (declare (ignore method))
                     (do-it (next-block block new-level) new-level)
                     (setf rest remaining))
                   (dolist (marker (reverse markup))
                     (format *output-stream* "</~A>" marker))
                   #+(or)
                   (format *output-stream* "~%"))))
    (when (document-property "html")
      (generate-html-header))
    (do-it (collect-elements (chunks document)) 
           (level document))
    (when (document-property "html")
      (format *output-stream* "~&</body>~&</html>"))))
    
;;; ---------------------------------------------------------------------------

(defun generate-html-header ()
  (generate-doctype)
  (format *output-stream* "~&<html>~&<head>")
  (awhen (document-property "title")
    (format *output-stream* "~&<title>~a</title>" it))
  (awhen (document-property "style-sheet")
    (unless (search ".css" it)
      (setf it (concatenate 'string it ".css")))
    (format *output-stream* "~&<link type='text/css' href='~a' rel='stylesheet' />" it))
  (format *output-stream* "~&</head>~&<body>"))

;;; ---------------------------------------------------------------------------

(defun generate-doctype ()
  (format *output-stream* "~&<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
        \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"))


