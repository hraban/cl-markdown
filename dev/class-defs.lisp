(in-package #:cl-markdown)

(defclass* document ()
  ((chunks (make-container 'vector-container) r)
   (link-info (make-container 'simple-associative-container
                              :test #'equal) r)
   (level 0 a)
   (markup nil a)
   (properties (make-container 'alist-container
                               :test #'string-equal) r)))

;;; ---------------------------------------------------------------------------

(defmethod document-property (name)
  (when *current-document*
    (item-at-1 (properties *current-document*) name)))

;;; ---------------------------------------------------------------------------

(defmethod (setf document-property) (value name)
  (if *current-document*
    (setf (item-at-1 (properties *current-document*) name) value)
    ;;?? weird since nothing happened
    (values value)))

;;; ---------------------------------------------------------------------------

(defclass* chunk ()
  ((lines (make-container 'vector-container) r)
   (blank-line-before? nil ia)
   (blank-line-after? nil ia)
   (started-by nil ia)
   (ended-by nil ia)
   (ignore? nil a)
   (markup-class nil ia)
   (indentation 0 ia)
   (level 0 ia)
   (paragraph? nil ia)
   (properties (make-container 'alist-container
                               :test #'string-equal) r)))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((object chunk) &key lines)
  (when lines
    (iterate-elements lines (lambda (line) (insert-item (lines object) line)))))

;;; ---------------------------------------------------------------------------

(defmethod print-object ((chunk chunk) stream)
  (print-unreadable-object (chunk stream :type t)
    (format stream "~A/~A ~D lines ~A ~A" 
            (markup-class chunk)
            (level chunk)
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
   (chunk-post-processors nil ia)
   (chunk-level 0 ia)
   (current-strip "" ia)
   (line-code->stripper (make-container 'simple-associative-container) r)
   (strippers (make-container 'stack-container) r)))
                 
;;; ---------------------------------------------------------------------------

(defun current-chunk-parser ()
  (chunk-parsing-environment *parsing-environment*))

;;; ---------------------------------------------------------------------------

(defclass* link-info ()
  ((url nil ir)
   (title nil ia)
   (id nil ia)))

;;; ---------------------------------------------------------------------------

(defmethod print-object ((object link-info) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A -> ~A" (id object) (url object))))
       