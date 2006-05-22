(in-package #:cl-markdown)

(declaim (optimize (speed 0) (space 0) (debug 3)))

;;; ---------------------------------------------------------------------------

(defparameter *markup->html*
  (make-container 
   'simple-associative-container
   :test #'equal
   :initial-contents 
   '((header1)    (nil :h1)
     (header2)    (nil :h2)
     (header3)    (nil :h3)
     (header4)    (nil :h4)
     (header5)    (nil :h5)
     (header6)    (nil :h6)
     
     (bullet)     ((:ul) :li)
     (code)       ((:pre :code) nil)
     (number)     ((:ol) :li)
     (quote)      ((:blockquote) nil)
     (horizontal-rule) (nil :hr))))

;;; ---------------------------------------------------------------------------

(defmethod render ((document document) (style (eql :html)) stream)
  (let ((*current-document* document)
        (*output-stream* stream))
    (setf (level document) 0
          (markup document) nil)
    (render-to-html document)))

;;; ---------------------------------------------------------------------------

(defun html-marker (chunk)
  (bind ((markup (markup-class-for-html chunk)))
    (first markup)))

;;; ---------------------------------------------------------------------------

(defmethod render-to-html ((chunk chunk))
  (bind ((markup (second (markup-class-for-html chunk)))
         (paragraph? (paragraph? chunk)))
    (encode-html chunk markup (when paragraph? 'p))))
  
;;; ---------------------------------------------------------------------------
  
(defun encode-html (stuff &rest codes)
  (declare (dynamic-extent codes))
  (cond ((null codes) 
         (iterate-elements
          (lines stuff)
          (lambda (line)
            (render-to-html line))))
        ((null (first codes))
         (apply #'encode-html stuff (rest codes)))
        (t (format *output-stream* "<~A>" (first codes))
           (apply #'encode-html stuff (rest codes))
           (format *output-stream* "</~A>" (first codes))))
  (format *output-stream* "~&"))

;;; ---------------------------------------------------------------------------

(defmethod markup-class-for-html ((chunk chunk))
  (when (markup-class chunk)
    (let ((translation (item-at-1 *markup->html* (markup-class chunk))))
      (unless translation 
        (warn "No translation for '~A'" (markup-class chunk)))
      translation)))

;;; ---------------------------------------------------------------------------

(defgeneric render-to-html (stuff)
  (:documentation ""))

;;; ---------------------------------------------------------------------------

(defmethod render-to-html ((chunk list))
  (render-span-to-html (first chunk) (rest chunk)))

;;; ---------------------------------------------------------------------------

(defmethod render-to-html ((chunk string))
  (format *output-stream* "~A" chunk))

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

(defmethod render-span-to-html ((code (eql 'code)) body)
  (format *output-stream* "<code>")
  (render-to-html (first body))
  (format *output-stream* "</code>"))

;;; ---------------------------------------------------------------------------

(defmethod render-span-to-html ((code (eql 'entity)) body)
  (output-html body))

;;; ---------------------------------------------------------------------------

(defmethod render-span-to-html ((code (eql 'reference-link)) body)
  (bind (((text &optional (id text)) body)
         (link-info (item-at-1 (link-info *current-document*) id)))
    (if link-info
      (output-link (url link-info) (title link-info) text)
      (output-html text))))

;;; ---------------------------------------------------------------------------

(defmethod render-span-to-html ((code (eql 'inline-link)) body)
  (bind (((text  &optional (url "") title) body))
    (output-link url title text)))

;;; ---------------------------------------------------------------------------

(defmethod render-span-to-html ((code (eql 'link)) body)
  (output-link body nil body))

;;; ---------------------------------------------------------------------------

(defun output-link (url title text)
  (if url
    (format *output-stream* "<a href=\"~A\" ~@[title=\"~A\"~]>~A</a>"
            url title text)
    (output-html *output-stream*)))

;;; ---------------------------------------------------------------------------

(defmethod render-span-to-html ((code (eql 'html)) body)
  ;; hack!
  (output-html (list (html-encode:encode-for-pre (first body)))))



#|
(setf *current-document* ccl:!)

(collect-elements
 (chunks *current-document*)
 :filter
 (lambda (c)
   (not (every-element-p
         (lines c)
         (lambda (l)
           (or (stringp l)
               (and (consp l) (length-at-most-p l 2))
               (and (consp l) (eq (first l) 'reference-link))))))))

(render-to-html ccl:@)

|#

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
                   (format *output-stream* "~%"))))
    (do-it (collect-elements (chunks document)) 
           (level document))))
    