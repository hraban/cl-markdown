(in-package #:cl-markdown)

(defvar *current-span* nil)
          
(defstruct (markdown-scanner (:conc-name scanner-))
  name regex priority function)

(setf (item-at-1 *spanner-parsing-environments* 'default)
      (make-instance 
       'sorted-list-container
       :sorter '<
       :key 'scanner-priority
       :initial-contents
       `(,(make-markdown-scanner 
	  :regex (create-scanner '(:sequence escaped-character))
	  :name 'escaped-character
	  :priority 1
	  :function 'convert-escape-temporarily)
	 ,(make-markdown-scanner
	  :regex (create-scanner '(:sequence inline-image))
	  :name 'inline-image
	  :priority 2)
	 ,(make-markdown-scanner
	   :regex (create-scanner
		   '(:sequence reference-image))
	   :name 'reference-image
	   :priority 3)
	  ;; must be before link
	  ,(make-markdown-scanner 
	    :regex (create-scanner '(:sequence anchor-with-text))
	    :name 'anchor-with-text
	    :priority 3.1)
	  ,(make-markdown-scanner 
	    :regex (create-scanner '(:sequence simple-anchor))
	    :name 'simple-anchor
	    :priority 3.2)
	 ,(make-markdown-scanner :regex (create-scanner
					 '(:sequence coded-reference-link))
				:name 'code
				:priority 4)
	 ,(make-markdown-scanner :regex (create-scanner
					 '(:sequence inline-link))
				:name 'inline-link
				:priority 5)
	 ,(make-markdown-scanner :regex (create-scanner
					 '(:sequence reference-link))
				:name 'reference-link
				:priority 6)
	 ,(make-markdown-scanner :regex (create-scanner '(:sequence backtick))
				:name 'code
				:priority 7)
	 ,(make-markdown-scanner :regex (create-scanner
					 '(:sequence strong-em-1))
				:name 'strong-em
				:priority 8)
	 ,(make-markdown-scanner :regex (create-scanner
					 '(:sequence strong-em-2))
				:name 'strong-em
				:priority 9)
	 ,(make-markdown-scanner :regex (create-scanner '(:sequence strong-2))
				:name 'strong
				:priority 10)
	 ,(make-markdown-scanner :regex (create-scanner '(:sequence strong-1))
				:name 'strong
				:priority 11)
	 ,(make-markdown-scanner :regex (create-scanner '(:sequence emphasis-2))
				:name 'emphasis
				:priority 12)
	 ,(make-markdown-scanner :regex (create-scanner '(:sequence emphasis-1))
				:name 'emphasis
				:priority 13)
	 ,(make-markdown-scanner :regex (create-scanner '(:sequence auto-link))
				:name 'link
				:priority 14)
	 ,(make-markdown-scanner :regex (create-scanner '(:sequence auto-mail))
				:name 'mail
				:priority 15)
	 ,(make-markdown-scanner :regex (create-scanner '(:sequence entity))
				:name 'entity
				:priority 16)
	 ,(make-markdown-scanner :regex (create-scanner '(:sequence html))
				:name 'html
				:priority 7.5)
	 ,(make-markdown-scanner 
	   :regex (create-scanner '(:sequence line-ends-with-two-spaces))
	   :name 'break
	   :priority 1.8)
	  )))

(setf (item-at-1 *spanner-parsing-environments* '(code))
      (make-instance 
       'sorted-list-container
       :sorter '<
       :key 'scanner-priority
       :initial-contents
      `(,(make-markdown-scanner 
	 :regex (create-scanner '(:sequence html))
	 :name 'html
	 :priority 1)
	,(make-markdown-scanner
	 :regex (create-scanner '(:sequence entity))
	 :name 'entity
	 :priority 2))))

(defun scanners-for-chunk (chunk)
  (let ((it nil))
  (cond ((setf it (item-at-1 *spanner-parsing-environments*
			     (markup-class chunk)))
	 (values it (markup-class chunk)))
	(t
	 (values (item-at-1 *spanner-parsing-environments* 'default) nil)))))

(defmethod handle-spans ((document abstract-document))
  (iterate-chunks
   document
   (lambda (chunk)
     (handle-spans chunk)))
  document)

(defmethod handle-spans ((chunk chunk)) 
  (setf (slot-value chunk 'lines)
        (bind ((lines (slot-value chunk 'lines))
               ((:values scanners kind) (scanners-for-chunk chunk))
               (*current-span* kind))
          (scan-lines-with-scanners lines scanners)))
  chunk)

(defun scan-lines-with-scanners (lines scanners)
  (when (or (consp lines) 
	    (typep lines 'cl-containers:iteratable-container-mixin))
    (iterate-elements
     scanners
     (lambda (scanner)
       (let ((name (scanner-name scanner)))
	 (setf lines
	       (let ((result nil))
		 (iterate-elements
		  lines
		  (lambda (line) 
		    (setf result 
			  (append result (scan-one-span
					  line name scanner scanners)))))
		 result))))))
  lines)

(defmethod scan-one-span ((line (eql nil)) scanner-name scanner scanners)
  (declare (ignorable scanner-name scanner scanners))
  (list ""))

(defmethod scan-one-span ((line cons) scanner-name scanner scanners)
  ;;?? what special case does this handle?
  (if (process-span-in-span-p scanner-name (first line))
    `((,(first line) 
       ,@(let ((*current-span* (first line)))
           (scan-one-span (second line) scanner-name scanner scanners))
       ,@(nthcdr 2 line)))
    (list line)))

(defmethod process-span-in-span-p ((sub-span t) (current-span t)) 
  (values t))

(defmethod process-span-in-span-p
    ((sub-span (eql nil)) (current-span (eql 'html))) 
  (values nil))

(defmethod process-span-in-span-p ((sub-span t) (current-span (eql 'html))) 
  (values nil))

(defmethod process-span-in-span-p ((sub-span (eql 'html)) (current-span t)) 
  (values nil))

(defmethod process-span-in-span-p ((sub-span (eql 'html)) (current-span null)) 
  (values t))

(defmethod process-span-in-span-p 
    ((sub-span (eql 'link)) (current-span (eql 'code))) 
  (values nil))

(defmethod process-span-in-span-p 
    ((sub-span (eql 'html)) (current-span (eql 'code))) 
  (values nil))

(defmethod process-span-in-span-p ((sub-span t) (current-span (eql 'code))) 
  (values nil))

(defmethod process-span-in-span-p 
    ((sub-span t) (current-span (eql 'coded-reference-link))) 
  (values nil))

(defmethod scan-one-span ((line string) scanner-name scanner scanners)
  #+debug
  (print (list :sos scanner-name *current-span*
	       (process-span-in-span-p scanner-name *current-span*)
	       line))
  (let ((found? nil)
	(result nil)
	(last-e 0)
	(regex (scanner-regex scanner))
	(scanner-fn (scanner-function scanner)))
    (when (process-span-in-span-p scanner-name *current-span*)
      (flet ((sub-scan (it)
	       (let ((*current-span* scanner-name))
		 (scan-lines-with-scanners it scanners))))
	(do-scans (s e gs ge regex line)
	  (let ((registers (loop for s-value across gs
			      for e-value across ge 
			      when (and (not (null s-value))
					(/= s-value e-value)) collect
			      (sub-scan (subseq line s-value e-value)))))
	    (setf registers (process-span scanner-name registers))
	    (let ((converted
		   `(,@(when (plusp s) 
			     `(,(sub-scan (subseq line last-e s))))
		       ,(if scanner-fn
			    (funcall scanner-fn scanner-name registers)
			    `(,scanner-name ,@registers)))))
	      (setf found? t
		    last-e e
		    result (append result converted)))))
	(when found?
	  (setf result
		(let ((last (sub-scan (subseq line last-e))))
		  (if (plusp (size last))
		      (append result (list last))
		      result)))
	  (return-from scan-one-span
	    (values (combine-strings result) t))))))
    (values (list line) nil))

(defun combine-strings (list)
  (let ((result nil)
	(current nil))
    (flet ((maybe-add (something)
	     (when something
	       (setf result (nconc result (list something))))))
      (iterate-elements
       list
       (lambda (elt)
	 (cond ((stringp elt)
		(if current
		    (setf current (concatenate 'string current elt))
		    (setf current elt)))
	       (t
		(maybe-add current)
		(maybe-add elt)
		(setf current nil)))))
      (maybe-add current)
      result)))

#+(or)
(ensure-same (combine-strings '("a" "b" 23 "c" "d")) ("ab" 23 "cd"))

#+(or)
(ensure-same (combine-strings '("a" "b" 23 2)) ("ab" 23 2))
       
#+(or)
(ensure-same (combine-strings '(1 2 3 )) (1 2 3))

#+(or)
(defmethod scan-one-span ((line string) scanner-name scanner scanners)
  (let ((found? nil)
	(result nil)
	(last-e 0)
	(regex (scanner-regex scanner))
	(scanner-fn (scanner-function scanner))
	(last-thing nil))
    (when (process-span-in-span-p scanner-name *current-span*)
      (flet ((sub-scan (it)
	       (let ((*current-span* scanner-name))
		 (scan-lines-with-scanners it scanners))))
	(do-scans (s e gs ge regex line)
	  (let ((registers (loop for s-value across gs
			      for e-value across ge 
			      when (and (not (null s-value))
					(/= s-value e-value)) collect
			      (sub-scan (subseq line s-value e-value)))))
	    (setf registers (process-span scanner-name registers))
	    (let ((converted
		   `(,@(when (plusp s) 
			     `(,(sub-scan (subseq line last-e s))))
		       ,(if scanner-fn
			    (funcall scanner-fn scanner-name registers)
			    `(,scanner-name ,@registers)))))
	      (print (list :c converted last-thing))
	      (cond ((and (stringp converted) 
			  (stringp last-thing))
		     (setf (first (last result)) 
			   (concatenate 'string last-thing converted)))
		    (t 
		     (setf result (append result converted))))
	      (setf found? t
		    last-e e
		    last-thing converted))))
	(when found?
	  (return-from scan-one-span
	    (values (let ((last (sub-scan (subseq line last-e))))
		      (if (plusp (size last))
			  (append result (list last))
			  result))
		    t))))))
    (values (list line) nil))

(defun convert-escape-temporarily (scanner-name registers)
  (declare (ignore scanner-name))
  (assert (position (aref (first registers) 0) *escape-characters*))
  (format nil "~c~c~a~c~c" 
	  #\Null #\Null 
	  (position (aref (first registers) 0) *escape-characters*)
	  #\Null #\Null))

(defmethod unconvert-escapes ((thing t))
  thing)

(defmethod unconvert-escapes ((string string))
  (cl-ppcre:regex-replace-all 
   '(:sequence escape-kludge) string 
   (lambda (_ &rest registers)
     (declare (ignore _)
	      (dynamic-extent registers))
     ;(print registers)
     (let ((ch (parse-integer (first registers))))
       (string (aref *escape-characters* ch))))
   :simple-calls t))

(defmethod unconvert-escapes ((thing list))
 (collect-elements thing :transform #'unconvert-escapes))

(defmethod unconvert-escapes ((thing chunk))
  (setf (slot-value thing 'lines)
	(collect-elements (lines thing) :transform #'unconvert-escapes)))

(defmethod unconvert-escapes ((thing abstract-document))
  (iterate-elements (chunks thing) #'unconvert-escapes))
