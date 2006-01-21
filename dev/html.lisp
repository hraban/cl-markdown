(in-package cl-markdown)

(defparameter *markup->html* (make-container 
                              'simple-associative-container
                              :test #'equal
                              :initial-contents 
                              '((paragraph) (nil "<P>" "</P>" nil)
                                (bullet)    ("<UL>" "<LI>" "</LI>" "</UL>")
                                (bullet paragraph)    ("<UL>" "<LI><P>" "</P></LI>" "</UL>"))))

;;; ---------------------------------------------------------------------------

(defgeneric render (document style stream)
  )

;;; ---------------------------------------------------------------------------

(defgeneric render-to-html (thing)
  )

;;; ---------------------------------------------------------------------------

(defvar *html-environment* nil)
  
(defmethod render ((document document) (style (eql :html)) stream)
  (let ((*html-environment* nil))
    (lml2:html-stream 
     stream
     ;;?? header
     (:body
      (iterate-elements 
       (chunks document)
       (lambda (chunk)
         (render-to-html chunk)))))))

;;; ---------------------------------------------------------------------------

(defmethod render-to-html ((chunk chunk))
  (let ((changed? (start-markup chunk)))
    (iterate-elements
     (lines chunk)
     (lambda (line)
       (render-to-html line)))
    (end-markup chunk changed?)))

;;; ---------------------------------------------------------------------------

(defun start-markup (chunk)
  (let ((changed? nil)
        (translation (item-at-1 *markup->html* (markup-classes chunk))))
    (unless translation
      (warn "Unknown translation ~A" (markup-classes chunk)))
    (format t "~%~A, ~A" (first *html-environment*) (markup-classes chunk))
    (unless (equal (first *html-environment*) (markup-classes chunk))
      (setf changed? t)
      (push (markup-classes chunk) *html-environment*)
      (safe-print (first translation)))
    (safe-print (second translation))
    (values changed?)))
  
;;; ---------------------------------------------------------------------------

(defun end-markup (chunk changed?)
  (let ((translation (item-at-1 *markup->html* (markup-classes chunk))))
    (safe-print (third translation))
    (when changed?
      (safe-print (fourth translation))
      (pop *html-environment*))))

;;; ---------------------------------------------------------------------------

(defmethod render-to-html ((line string))
  (safe-print line)
  ;;?? bit of a hack
  (safe-print " "))

;;; ---------------------------------------------------------------------------

(defun safe-print (thing)
  (when thing
    (lml2:lml-princ thing)))
                           