(in-package cl-markdown)

(defmacro with-stream-from-specifier ((stream stream-specifier) &body body)
  (with-gensyms (s close? output)
    `(let (,s (,close? t) ,output)
       (unwind-protect
         (setf ,output
               (prog1
                 (let (,stream)
                   (setf (values ,s ,close?) (make-stream-from-specifier ,stream-specifier)
                         ,stream ,s)
                   ,@body)
                 #+Ignore
                 (format t "~%~%~A ~A" ,close? ,s)))
         (when (and ,close? ,s)
           (awhen (close-stream-specifier ,s)
             (setf ,output it))))
       ,output))) 

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
  (values stream-specifier nil))

(defmethod make-stream-from-specifier ((stream-specifier (eql t)))
  (values *standard-output* nil))

(defmethod make-stream-from-specifier ((stream-specifier (eql nil)))
  (values (make-string-output-stream) t))

(defmethod make-stream-from-specifier ((stream-specifier (eql :none)))
  (values nil nil))

(defmethod make-stream-from-specifier ((stream-specifier pathname))
  (values (open stream-specifier :direction :output :if-exists :supersede)
          t))

