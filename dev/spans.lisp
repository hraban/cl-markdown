(in-package #:cl-markdown)

(defvar *current-span* nil)
          
(defstruct (markdown-scanner (:conc-name scanner-))
  name regex priority)

(setf (item-at-1 *spanner-parsing-environments* 'default)
      (make-instance 
       'sorted-list-container
       :sorter '<
       :key 'scanner-priority
       :initial-contents
       `(,(make-markdown-scanner 
	  :regex (create-scanner '(:sequence escaped-character))
	  :name 'escaped-character
	  :priority 1)
	 ,(make-markdown-scanner
	  :regex (create-scanner '(:sequence inline-image))
	  :name 'inline-image
	  :priority 2)
	 ,(make-markdown-scanner :regex (create-scanner
					 '(:sequence reference-image))
				:name 'reference-image
				:priority 3)
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
	 ,(make-markdown-scanner :regex (create-scanner '(:sequence html))
				:name 'html
				:priority 17)
	 ,(make-markdown-scanner :regex (create-scanner '(:sequence entity))
				:name 'entity
				:priority 16))))

(setf (item-at-1 *spanner-parsing-environments* '(code))
      `(,(make-markdown-scanner 
	 :regex (create-scanner '(:sequence html))
	 :name 'html
	 :priority 1)
	,(make-markdown-scanner
	 :regex (create-scanner '(:sequence entity))
	 :name 'entity
	 :priority 2)))

(defun scanners-for-chunk (chunk)
  (acond ((item-at-1 *spanner-parsing-environments* (markup-class chunk))
          (values it (markup-class chunk)))
         (t
          (values (item-at-1 *spanner-parsing-environments* 'default) nil))))

(defmethod handle-spans ((document document))
  (iterate-elements
   (chunks document)
   (lambda (chunk)
     (handle-spans chunk)))
  document)

(defmethod handle-spans ((chunk chunk)) 
  (setf (slot-value chunk 'lines)
        (bind ((lines (slot-value chunk 'lines))
               ((values scanners kind) (scanners-for-chunk chunk))
               (*current-span* kind))
          (scan-lines-with-scanners lines scanners)))
  chunk)

(defun scan-lines-with-scanners (lines scanners)
  (when (consp lines)
    (iterate-elements
     scanners
     (lambda (scanner)
       (let ((regex (scanner-regex scanner))
	     (name (scanner-name scanner)))
	 (setf lines
	       (let ((result nil))
		 (iterate-elements
		  lines
		  (lambda (line) 
		    (setf result 
			  (append result (scan-one-span
					  line name regex scanners)))))
		 result))))))
  lines)

;;; ---------------------------------------------------------------------------

(defmethod scan-one-span ((line (eql nil)) name regex scanners)
  (declare (ignorable name regex scanners))
  (list ""))

;;; ---------------------------------------------------------------------------

(defmethod scan-one-span ((line cons) name regex scanners)
  ;;?? what special case does this handle?
  (if (process-span-in-span-p name (first line))
    `((,(first line) 
       ,@(let ((*current-span* (first line)))
           (scan-one-span (second line) name regex scanners))
       ,@(nthcdr 2 line)))
    (list line)))

;;; ---------------------------------------------------------------------------

(defmethod process-span-in-span-p ((span-1 (eql nil)) (span-2 (eql 'html))) 
  (values nil))

;;; ---------------------------------------------------------------------------

(defmethod process-span-in-span-p ((span-1 t) (span-2 t)) 
  (values t))

;;; ---------------------------------------------------------------------------

(defmethod process-span-in-span-p ((span-1 (eql 'link)) (span-2 (eql 'code))) 
  (values nil))

;;; ---------------------------------------------------------------------------

(defmethod process-span-in-span-p ((span-1 (eql 'html)) (span-2 t)) 
  (values nil))

;;; ---------------------------------------------------------------------------

(defmethod process-span-in-span-p ((span-1 (eql 'html)) (span-2 (eql 'code))) 
  (values t))

;;; ---------------------------------------------------------------------------

(defmethod process-span-in-span-p ((span-2 t) (span-1 (eql 'code))) 
  (values nil))

;;; ---------------------------------------------------------------------------

(defmethod process-span-in-span-p ((span-2 t) (span-1 (eql 'coded-reference-link))) 
  (values nil))

;;; ---------------------------------------------------------------------------

(defmethod scan-one-span ((line string) name regex scanners)
  (when (process-span-in-span-p name *current-span*)
    (let ((found? nil)
          (result nil)
          (last-e 0))
      (flet ((sub-scan (it)
               (let ((*current-span* name))
                 (scan-lines-with-scanners it scanners))))
        (do-scans (s e gs ge regex line)
          (let ((registers (loop for s-value across gs
                                 for e-value across ge 
                                 when (and (not (null s-value))
                                           (/= s-value e-value)) collect
                                 (sub-scan (subseq line s-value e-value)))))
            (setf registers (process-span name registers))
            (setf found? t
                  result (append result
                                 `(,@(when (plusp s) 
                                       `(,(sub-scan (subseq line last-e s))))
                                   (,name ,@registers)))
                  last-e e)))
        (when found?
          (return-from scan-one-span
            (values (let ((last (sub-scan (subseq line last-e))))
                      (if (plusp (size last))
                        (append result (list last))
                        result))
                    t))))))
    (values (list line) nil))

 