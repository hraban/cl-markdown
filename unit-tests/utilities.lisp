(in-package #:cl-markdown-test)

(defun strip-html (string)
  (with-output-to-string (out)
    (flet ((emit (ch)
	     (write-char ch out)))
      (let ((quote? nil)
	    (bracket? nil)
	    (index 0))
	(loop while (< index (length string)) do
	   (let ((ch (aref string index)))
	     (cond ((char= ch #\\)
		    (emit ch)
		    (emit (aref string (incf index))))
		   ((char= ch #\")
		    (setf quote? (not quote?))
		    (emit ch))
		   ((and (not quote?) (char= ch #\<))
		    (setf bracket? t))
		   ((and bracket? (char= ch #\>))
		    (setf bracket? nil))
		   (bracket?
		    ;; skip it
		    )
		   (t
		    (emit ch)))
	     (incf index)))))))

(defun compare-line-by-line (a b &key (key 'identity) (test 'string=))
  (setf key (coerce key 'function))
  (setf test (coerce test 'function))
  (let ((ia (make-iterator a :treat-contents-as :lines))
	(ib (make-iterator b :treat-contents-as :lines)))
    (map-containers
     (lambda (la lb)
       (unless (funcall test (funcall key la) (funcall key lb))
	 (return-from compare-line-by-line nil)))
     ia ib)
    (and (null (move-forward-p ia)) (null (move-forward-p ib)))))

#+(or)
(defun compare-line-by-line (a b &key (key 'identity) (test 'string=))
  (setf key (coerce key 'function))
  (setf test (coerce test 'function))
  (with-input-from-string (sa a)
    (with-input-from-string (sb b)
      (loop for la = (read-line sa nil nil)
	 for lb = (read-line sb nil nil)
	 when (and (not la) (not lb)) do (return t)
	 when (or (not la) (not lb)
		  (not (funcall test (funcall key la) (funcall key lb)))) do
	   (return nil)
	 finally (return t)))))

#+(or)
(compare-line-by-line
 "a
b
c"
"a
b
c"
)
