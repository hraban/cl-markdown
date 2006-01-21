(in-package cl-markdown)

(defclass* document ()
  ((chunks (make-container 'vector-container) r)))

;;; ---------------------------------------------------------------------------

(defclass* chunk ()
  ((lines (make-container 'vector-container) r)
   (started-by nil ia)
   (ended-by nil ia)
   (ignore? nil a)
   (markup-classes nil ia)
   (indentation 0 ia)))

;;; ---------------------------------------------------------------------------

(defmethod print-object ((chunk chunk) stream)
  (print-unreadable-object (chunk stream :type t)
    (format stream "~A ~D lines ~A ~A" 
            (markup-classes chunk)
            (size (lines chunk))
            (started-by chunk)
            (ended-by chunk))))

;;; ---------------------------------------------------------------------------

(defclass* chunk-parsing-environment ()
  ((chunk-enders nil ia)
   (chunk-starters nil ia)
   (line-coders nil ia)
   (parser-map nil ia)))

;;; ---------------------------------------------------------------------------

(defclass* parsing-environment ()
  ((chunk-parsing-environment nil a)
   (chunk-post-processors nil ia)))
                 
;;; ---------------------------------------------------------------------------

(defun current-chunk-parser ()
  (chunk-parsing-environment *parsing-environment*))