(in-package cl-markdown)

(defmacro with-stream-from-specifier ((stream stream-specifier) &body body)
  `(let (s close? output)
     (unwind-protect
       (setf output
             (let (,stream)
               (setf (values s close?) (make-stream-from-specifier ,stream-specifier)
                     ,stream s
                     close? t)
               ,@body))
       (when (and close? s)
         (awhen (close-stream-specifier s)
           (setf output it))))
     output)) 

(defmethod close-stream-specifier (s)
  (close s)
  (values nil))

(defmethod close-stream-specifier ((s ccl:string-output-stream))
  (prog1 
    (values (get-output-stream-string s)) 
    (close s)))

(defmethod render-to-stream (document style stream-specifier)
  (with-stream-from-specifier (stream stream-specifier)
    (render document style stream)))
  
(defmethod make-stream-from-specifier ((stream-specifier stream))
  stream-specifier)

(defmethod make-stream-from-specifier ((stream-specifier (eql t)))
  *standard-output*)

(defmethod make-stream-from-specifier ((stream-specifier (eql nil)))
  (make-string-output-stream))

(defmethod make-stream-from-specifier ((stream-specifier (eql :none)))
  nil)

(defmethod make-stream-from-specifier ((stream-specifier pathname))
  (open stream-specifier :direction :output :if-exists :supersede))

