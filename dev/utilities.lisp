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
    (cond ((null pos-level)
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
           (setf deeper (subseq chunks 0 (+ pos-markup 2))
                 chunks (nthcdr (1+ pos-markup) chunks)
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
  ;;(break)
  (let ((*print-readably* nil))
    (fresh-line *debug-io*)
    (apply #'format *debug-io* msg args)
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
