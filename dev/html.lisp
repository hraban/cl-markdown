(in-package #:cl-markdown)

;;; dealing with 'levels'

(defparameter *current-document* nil)

#+Ignore
(defun d (text)
  (let* ((document (markdown text))
        (*current-document* document))
    (setf (level document) 0
          (markup document) nil)
    (collect-elements (chunks document))))

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
  (let ((*current-document* document))
    (setf (level document) 0
          (markup document) nil)
    (let* ((chunks (collect-elements (chunks document)))
           (result (html-list->tree chunks)))
      (if stream
        (format stream "~S" result)
        result))))

;;; ---------------------------------------------------------------------------

(defmethod render ((document document) (style (eql :html)) stream)
  (eval `(html:html-stream 
          ,stream 
          (html:html ,@(render-to-stream document :html :none)))))

;;; ---------------------------------------------------------------------------

(defun html-marker (chunk)
  (bind ((markup (markup-class-for-html chunk)))
    (first markup)))

;;; ---------------------------------------------------------------------------

(defmethod render-to-html ((chunk chunk))
  (bind ((block (collect-elements
                 (lines chunk)
                 :transform (lambda (line)
                              (render-to-html line))))
         (markup (second (markup-class-for-html chunk)))
         (paragraph? (paragraph? chunk)))
    (cond ((and paragraph? markup)
           (values `(,markup (:P ,@block)) t))
          (paragraph?
           (values `(:P ,@block) t))
          (markup
           (values `(,markup ,@block) t))
          (t
           (values block nil)))))

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

(defmethod render-to-html ((chunk string))
  ;;?? unlovely
  (format nil "~A" chunk))

;;; ---------------------------------------------------------------------------

(defmethod render-span-to-html ((code (eql 'strong)) body)
  `(:strong ,@body))

;;; ---------------------------------------------------------------------------

(defmethod render-span-to-html ((code (eql 'mail)) body)
  (let ((address (first body)))
    `((:a :href ,(format nil "mailto:~A" address)) ,address)))

;;; ---------------------------------------------------------------------------

(defmethod render-span-to-html ((code (eql 'emphasis)) body)
  `(:em ,@body))

;;; ---------------------------------------------------------------------------

(defmethod render-span-to-html ((code (eql 'strong-em)) body)
  `(:strong (:em ,@body)))

;;; ---------------------------------------------------------------------------

(defmethod render-span-to-html ((code (eql 'code)) body)
  `(:code ,(render-to-html (first body))))

;;; ---------------------------------------------------------------------------

(defmethod render-span-to-html ((code (eql 'entity)) body)
  (first body))

;;; ---------------------------------------------------------------------------

(defmethod render-span-to-html ((code (eql 'reference-link)) body)
  (bind (((text &optional (id text)) body)
         (link-info (item-at-1 (link-info *current-document*) id)))
    (if link-info
      `((:a :href ,(url link-info) ,@(awhen (title link-info) `(:title ,it)))
        ,text)
      `,text)))

;;; ---------------------------------------------------------------------------

(defmethod render-span-to-html ((code (eql 'inline-link)) body)
  (bind (((text  &optional (url "") title) body))
    `((:a :href ,url ,@(awhen title `(:title ,it)))
      ,text)))

;;; ---------------------------------------------------------------------------

(defmethod render-span-to-html ((code (eql 'link)) body)
  (bind ((url body))
    `((:a :href ,@url) ,@url)))

;;; ---------------------------------------------------------------------------

(defmethod render-span-to-html ((code (eql 'html)) body)
  (html-encode:encode-for-pre (first body)))




