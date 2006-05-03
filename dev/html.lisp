(in-package cl-markdown)

(defparameter *markup->html* (make-container 
                              'simple-associative-container
                              :test #'equal
                              :initial-contents 
                              '((paragraph) (nil "<P>" "</P>" nil)
                                (header1)    (nil "<H1>" "</H1>" nil)
                                (bullet)    ("<UL>" "<LI>" "</LI>" "</UL>")
                                (bullet paragraph)    ("<UL>" "<LI><P>" "</P></LI>" "</UL>")
                                (code) ("<PRE><CODE>" nil nil "</CODE></PRE>")
                                (number)    ("<OL>" "<LI>" "</LI>" "</OL>")
                                (number paragraph)    ("<OL>" "<LI><P>" "</P></LI>" "</OL>")
                                (paragraph quote) ("<BLOCKQUOTE>" "<P>" "</P>" "</BLOCKQUOTE>")
                                (quote) ("<BLOCKQUOTE>" nil nil "</BLOCKQUOTE>")
                                
                                code (nil "<CODE>" "</CODE>" nil)
                                strong (nil "<STRONG>" "</STRONG>" nil)
                                emphasis (nil "<EM>" "</EM" nil))))

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
  (let ((finish (start-markup chunk)))
    (iterate-elements
     (lines chunk)
     (lambda (line)
       (render-to-html line)))
    (safe-print finish)))

;;; ---------------------------------------------------------------------------

(defmethod render-to-html ((chunk list))
  (let ((translation (item-at-1 *markup->html* (first chunk))))
    (unless translation
      (warn "Unknown translation ~A" (first chunk)))
    (safe-print (second translation))
    (safe-print (second chunk))
    (safe-print (third translation))))
  
;;; ---------------------------------------------------------------------------

(defun start-markup (chunk)
  (let ((translation (item-at-1 *markup->html* (markup-class chunk)))
        (html-depth (size *html-environment*)))
    (unless translation
      (warn "Unknown translation ~A" (markup-class chunk)))
    ;(format t "~%~D ~D ~A : " (level chunk) html-depth translation) 
    
    ;; Unless more of the same, do nothing...
    (unless (and (= (1+ (level chunk)) html-depth)
                 (equal translation (first *html-environment*))) 
      (cond ((< (level chunk) html-depth)
             ;; back out...
             (loop repeat (- html-depth (level chunk)) do
                   (pop-markup)))
            ((>= (level chunk) html-depth) 
             (when (first translation)
               (push translation *html-environment*))
             (safe-print (first translation)))))
    
    (safe-print (second translation))
    (values (third translation))))
  
;;; ---------------------------------------------------------------------------

(defun pop-markup ()
  (assert (not (empty-p *html-environment*)))
  (let ((translation (pop *html-environment*)))
    (safe-print (fourth translation))))

;;; ---------------------------------------------------------------------------

(defmethod render-to-html ((line string))
  (safe-print line)
  ;;?? bit of a hack
  (safe-print " "))

;;; ---------------------------------------------------------------------------

(defun safe-print (thing)
  (when thing
    (lml2:lml-princ thing)))
                           