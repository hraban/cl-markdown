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

(defparameter *markup->lml2*
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

(defmethod render ((document document) (style (eql :lml2)) stream)
  (let ((*current-document* document))
    (setf (level document) 0
          (markup document) nil)
    (let* ((chunks (collect-elements (chunks document)))
           (result (lml2-list->tree chunks)))
      (if stream
        (format stream "~S" result)
        result))))

;;; ---------------------------------------------------------------------------

(defmethod render ((document document) (style (eql :html)) stream)
  (eval `(lml2:html-stream 
          ,stream 
          (lml2:html ,@(render-to-stream document :lml2 :none)))))

;;; ---------------------------------------------------------------------------

(defun lml2-marker (chunk)
  (bind ((markup (markup-class-for-lml2 chunk)))
    (first markup)))

;;; ---------------------------------------------------------------------------

(defmethod render-to-lml2 ((chunk chunk))
  (bind ((block (collect-elements
                 (lines chunk)
                 :transform (lambda (line)
                              (render-to-lml2 line))))
         (markup (second (markup-class-for-lml2 chunk)))
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

#+Ignore
(defun add-markup (stuff markup)
  (cond ((null markup)
         stuff)
        ((atom markup)
         `(,markup ,stuff))
        ((atom (first markup))
         `(,(first markup) ,stuff))
        ((consp (first markup))
         (loop for tag in (first markup) do
               (setf stuff (add-markup tag stuff)))
         stuff)
        (t
         (error "didn't think of this"))))

;;; ---------------------------------------------------------------------------

(defmethod markup-class-for-lml2 ((chunk chunk))
  (when (markup-class chunk)
    (let ((translation (item-at-1 *markup->lml2* (markup-class chunk))))
      (unless translation 
        (warn "No translation for '~A'" (markup-class chunk)))
      translation)))

;;; ---------------------------------------------------------------------------

(defmethod render-to-lml2 ((chunk list))
  (render-span-to-lml2 (first chunk) (rest chunk)))

;;; ---------------------------------------------------------------------------

(defmethod render-to-lml2 ((chunk string))
  ;;?? unlovely
  (format nil "~A" chunk))

;;; ---------------------------------------------------------------------------

(defmethod render-span-to-lml2 ((code (eql 'strong)) body)
  `(:strong ,@body))

;;; ---------------------------------------------------------------------------

(defmethod render-span-to-lml2 ((code (eql 'emphasis)) body)
  `(:em ,@body))

;;; ---------------------------------------------------------------------------

(defmethod render-span-to-lml2 ((code (eql 'strong-em)) body)
  `(:strong (:em ,@body)))

;;; ---------------------------------------------------------------------------

(defmethod render-span-to-lml2 ((code (eql 'code)) body)
  `(:code ,(render-to-lml2 (first body))))

;;; ---------------------------------------------------------------------------

(defmethod render-span-to-lml2 ((code (eql 'entity)) body)
  (first body))

;;; ---------------------------------------------------------------------------

(defmethod render-span-to-lml2 ((code (eql 'reference-link)) body)
  (bind (((text &optional (id text)) body)
         (link-info (item-at-1 (link-info *current-document*) id)))
    (if link-info
      `((:a :href ,(url link-info) ,@(awhen (title link-info) `(:title ,it)))
        ,text)
      `,text)))

;;; ---------------------------------------------------------------------------

(defmethod render-span-to-lml2 ((code (eql 'inline-link)) body)
  (bind (((text  &optional (url "") title) body))
    `((:a :href ,url ,@(awhen title `(:title ,it)))
      ,text)))

;;; ---------------------------------------------------------------------------

(defmethod render-span-to-lml2 ((code (eql 'link)) body)
  (bind ((url body))
    `((:a :href ,@url) ,@url)))

;;; ---------------------------------------------------------------------------

(defmethod render-span-to-lml2 ((code (eql 'html)) body)
  (html-encode:encode-for-pre (first body)))



