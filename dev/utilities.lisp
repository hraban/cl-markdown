(in-package #:cl-markdown)

(defun next-block (chunks level)
  (let* (;; the first chunk after the current one with a lower level
         (pos-level (position-if 
                     (lambda (other)
		       (< (level other) level))
                     (rest chunks)))
         ;; the first chunk after the current one with different markup
	 ;; and level less than or equal to
        (pos-markup (position-if 
                      (lambda (other)
			(and (<= (level other) level)
			     (not (samep (markup-class other) 
					 (markup-class (first chunks))))))
                      (rest chunks)))
         pos-style
         deeper)
    #+Ignore
    (format t "~%~d ~2,D:POS: ~A, ~A - ~A" 
            level (length chunks) pos-level pos-markup (first chunks))
    ;; remember that there will be another call to rest 
    (cond #+(or) ((null pos-level)
           ;; go all the way to the end
           (setf deeper (subseq chunks 0)
                 chunks nil
                 pos-style :rest))
          ((or (and pos-level pos-markup (> pos-level pos-markup))
               (and pos-level (not pos-markup)))
	   ;(format t " -- level")
	   (setf deeper (subseq chunks 0 (1+ pos-level))
                 chunks (nthcdr pos-level chunks)
                 pos-style :level))
          ((or (and pos-level pos-markup (> pos-markup pos-level))
               (and (not pos-level) pos-markup))
	   ;(format t " -- markup")
           (setf deeper (subseq chunks 0 (+ pos-markup 1))
                 chunks (nthcdr pos-markup chunks)
                 pos-style :markup))
          ((and pos-level pos-markup (= pos-level pos-markup))
	   ;(format t " -- level")	   
           (setf deeper (subseq chunks 0 (1+ pos-level))
                 chunks (nthcdr pos-level chunks)
                 pos-style :level))
          (t
           ;; nothing found, take the rest
           (setf deeper chunks 
                 chunks nil
                 pos-style :none)))
    ;(format t "~{~%    ~a~}" (collect-elements deeper))
    (values deeper chunks pos-style)))

(defmethod render-to-stream (document style stream-specifier)
  (with-stream-from-specifier (stream stream-specifier :output 
				      :if-exists :supersede)
    (let ((*current-document* document)
          (*current-format* style)
          (*output-stream* stream))
      (setf (level document) 0
            (markup document) nil)
      (render document style stream))))
  
(defun ensure-string (it)
  (typecase it
    (string it)
    (symbol (symbol-name it))
    (t (format nil "~a" it))))

(defun collect-links (document)
  (collect-key-value (link-info document)
		     :transform (lambda (name link)
				  (cons name (url link)))
		     :filter (lambda (k v)
			       (declare (ignore k))
			       (typep v 'link-info))))

(defun starts-with (string prefix)
  (let ((mismatch (mismatch prefix string)))
    (or (not mismatch) (= mismatch (length prefix)))))

(defun markdown-warning (msg &rest args)
  (let* ((*print-readably* nil)
	 (warning (apply #'format nil msg args)))
    (when *current-document*
      (push warning (warnings *current-document*)))
    (fresh-line *debug-io*)
    (write-string warning *debug-io*)
    (terpri *debug-io*)))

(eval-always 
(defun _mark-range (array start end)
  (loop for a from (char-code start) to (char-code end) do
       (setf (sbit array a) 1)))

(defun _mark-one (array ch)
  (setf (sbit array (char-code ch)) 1)))


(defparameter +first-name-characters+ 
  (let ((array (make-array 255 :element-type 'bit :initial-element 0)))
    (_mark-range array #\a #\z)
    (_mark-range array #\A #\Z)
    array))

(defparameter +name-characters+ 
  (let ((array (copy-seq +first-name-characters+)))
    (_mark-range array #\0 #\9)
    (_mark-one array #\_)
    (_mark-one array #\-)
    (_mark-one array #\.)
    (_mark-one array #\:)
    array))

(defun html-safe-name (name)
  ;; Copied from HTML-Encode
  ;;?? this is very consy
  ;;?? crappy name
  (declare (type simple-string name))
  (let ((output (make-array (truncate (length name) 2/3)
                            :element-type 'character
                            :adjustable t
                            :fill-pointer 0)))
    (with-output-to-string (out output)
      (loop for char across name
	 for code = (char-code char)
	 for valid = +first-name-characters+ then +name-characters+
	 do (cond ((and (< code 255)
			(= (sbit valid code) 1))
		   (write-char char out))
		  (t
		   (format out "x~d:." code)))))
    (coerce output 'simple-string)))


(defun string->list (string &key (stop-word-p (constantly nil))
			  (ignore-character-p (constantly nil)))
  (let ((word-array (make-array (length string) :element-type 'character))
	(results nil)
	(index 0)
	(in-quote-p nil))
    (labels ((grab-char (ch)
	       (setf (aref word-array index) ch)
	       (incf index))
	     (add-word (word)
	       (push word results))
	     (maybe-add-word ()
	       (let ((word (coerce (subseq word-array 0 index) 'string)))
		 (unless (funcall stop-word-p word)
		   (add-word (strip-whitespace word))))
	       (setf index 0)))
      (loop for i below (length string)
	 for ch = (aref string i) do
	 (cond ((char= ch #\\)
		(grab-char ch)
		(incf i)
		(grab-char (aref string i)))
	       ((char= ch #\")
		(setf in-quote-p (not in-quote-p))
		#+(or)
		(grab-char #\"))
	       ((and (not in-quote-p) (> index 0) (whitespacep ch))
		;; have a word
		(maybe-add-word))
	       ((and (not in-quote-p) (funcall ignore-character-p ch))
		)
	       (t
		(grab-char ch))))
      (when (> index 0)
	(maybe-add-word))
      (nreverse results))))

(defun string-trim-if (predicate string &key (start 0) (end (length string)))
  (let ((end (1- end)))
    (loop for ch across string 
       while (funcall predicate ch) do (incf start))
    (when (< start end)
      (loop for ch = (aref string end)
         while (funcall predicate ch) do (decf end)))
    (subseq string start (1+ end))))

(defun strip-whitespace (string &key (start 0) (end (length string)))
  (string-trim-if
   #'whitespacep string :start start :end end))

(defun process-brackets (document line-iterator)
  (bind ((output (make-array 4096
			     :element-type 'character
			     :adjustable t
			     :fill-pointer 0))
	 (depth 0)
	 (location 0)
	 (buffers (bracket-references document))
	 (buffer-count (size buffers))
	 (current-buffer nil))
    (with-output-to-string (out output)
      (block 
	  iteration-block
	(flet ((write-buffer-count-and-exit (count)
		 (format out " ~d}" count)
		 (return-from iteration-block nil))
	       (add-char (ch)
		 (if current-buffer
		     (vector-push-extend ch current-buffer)
		     (write-char ch out)))
	       (start-buffer ()
		 (setf current-buffer 
		       (make-array 4096
				   :element-type 'character
				   :adjustable t
				   :fill-pointer 0))))
	  (iterate-elements 
	   line-iterator
	   (lambda (line)
	     (with-iterator (iterator line 
				      :treat-contents-as :characters
				      :skip-empty-chunks? nil)
	       (iterate-elements
		iterator
		(lambda (ch)
		  ;;(print (list ch (not (null current-buffer))))
		  (incf location)
		  (cond ((char= ch #\\)
			 (unless (move-forward-p iterator)
			   (error "Invalid escape at char ~d" location))
			 (add-char ch)
			 (add-char (next-element iterator)))
			((and (= depth 1) (not current-buffer) (whitespacep ch))
			 ;; finished reading the command name 
			 ;; (don't write the ws)
			 (start-buffer))
			((char= ch #\{)
			 (incf depth)
			 (add-char ch))
			((char= ch #\})
			 (decf depth)
			 (cond ((= depth 0)
				(insert-item 
				 buffers  
				 (coerce current-buffer 'simple-string))
				(write-buffer-count-and-exit buffer-count))
			       ((< depth 0)
				;; FIXME -- an error
				(setf depth 0)))
			 (add-char ch))
			(t
			 (add-char ch))))))
	     (when (= depth 0)
	       ;; no brackets to process
	       (return-from iteration-block nil))
	     (if (and (= depth 1) (not current-buffer))
		 (start-buffer)
		 (add-char #\Newline))))))
      (coerce output 'simple-string))))

#+(or)
(defun process-brackets (document line-iterator)
  (bind ((output (make-array 4096
			     :element-type 'character
			     :adjustable t
			     :fill-pointer 0))
	 (depth 0)
	 (location 0)
	 (buffers (bracket-references document))
	 (buffer-count (size buffers))
	 (current-buffer nil)
	 (add-quote? nil)
	 (seen-whitespace? nil))
    (with-output-to-string (out output)
      (block 
	  iteration-block
	(flet ((write-buffer-count-and-exit (count)
		 (format out " ~d}" count)
		 (return-from iteration-block nil))
	       (add-char (ch)
		 (if current-buffer
		     (vector-push-extend ch current-buffer)
		     (write-char ch out)))
	       (start-buffer ()
		 (setf current-buffer 
		       (make-array 4096
				   :element-type 'character
				   :adjustable t
				   :fill-pointer 0))))
	  (iterate-elements 
	   line-iterator
	   (lambda (line)
	     (with-iterator (iterator line 
				      :treat-contents-as :characters
				      :skip-empty-chunks? nil)
	       (iterate-elements
		iterator
		(lambda (ch)
		  ;;(print (list ch (not (null current-buffer))))
		  (incf location)
		  (cond ((char= ch #\\)
			 (unless (move-forward-p iterator)
			   (error "Invalid escape at char ~d" location))
			 (add-char ch)
			 (add-char (next-element iterator)))
			((and (= depth 1) (not current-buffer))
			 (cond ((whitespacep ch)
				(setf seen-whitespace? t))
			       (seen-whitespace?
				;; finished reading the command name 
				;; (don't write the ws)
				(start-buffer)
				(unless (char= ch #\")
				  (setf add-quote? t)
				  (add-char #\"))))
			 (add-char ch))
			((char= ch #\{)
			 (incf depth)
			 (add-char ch))
			((char= ch #\})
			 (decf depth)
			 (cond ((= depth 0)
				;; (maybe) add final quote and grab it
				(when add-quote?
				  (add-char #\"))
				(insert-item 
				 buffers  
				 (coerce current-buffer 'simple-string))
				(write-buffer-count-and-exit buffer-count))
			       ((< depth 0)
				;; FIXME -- an error
				(setf depth 0)))
			 (add-char ch))
			(t
			 (add-char ch))))))
	     (when (= depth 0)
	       ;; no brackets to process
	       (return-from iteration-block nil))
	     (if (and (= depth 1) (not current-buffer))
		 (start-buffer)
		 (add-char #\Newline))))))
      (coerce output 'simple-string))))

#+ignore
(let  ((li (make-iterator "a b
c d e
f" :treat-contents-as :lines)))
  (iterate-elements 
   li
   (lambda (x)
     (iterate-elements li
		       (lambda (y)
     (let ((ci (make-iterator y :treat-contents-as :characters :skip-empty-chunks? nil)))
       (iterate-elements ci #'print)))))))