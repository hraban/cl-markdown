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
			     (markup-class other) ;paragraphs don't count
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

  ;; FIXME - bad name
  ;; this is the parent that is a child-document or document, not, e.g., 
  ;; an included-document
(defmethod main-parent ((document included-document))
  (main-parent (parent document)))

(defmethod main-parent ((document abstract-document))
  document)

(defun root-parent (document)
  (or (and (parent document)
	   (root-parent (parent document)))
      document))

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
      (push (cons (main-parent *current-document*) warning)
	    (warnings (root-parent *current-document*))))
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
                            :fill-pointer 0))
	(first? t))
    (with-output-to-string (out output)
      (loop for char across name
	 for code = (char-code char)
	 for valid = +first-name-characters+ then +name-characters+
	 do (cond ((and (< code 255)
			(= (sbit valid code) 1))
		   (write-char char out))
		  (t
		   ;; See http://www.w3.org/TR/html4/types.html#h-6.2
		   ;; ID and NAME tokens must begin with a letter ([A-Za-z]) 
		   ;; and may be followed by any number of letters, 
		   ;; digits ([0-9]), hyphens ("-"), underscores ("_"), 
		   ;; colons (":"), and periods (".").
		   (when first?
		     (write-char #\x out)) 
		   (format out ":~:@(~16,r~)" code)))
	   (setf first? nil)))
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

(defun process-brackets (document current-line line-iterator)
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
	(flet ((write-buffer-count (count)
		 (format out " ~d" count)
		 (setf current-buffer nil))
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
	  (flet ((process-brackets-in-line (line)
		   ;;(print (list :line  line))
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
			      ((and (= depth 1) (not current-buffer)
				    (whitespacep ch))
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
				      (write-buffer-count buffer-count))
				     ((< depth 0)
				      ;; FIXME -- an error
				      (setf depth 0)))
			       (add-char ch))
			      (t
			       (add-char ch))))))
		   ;; if no brackets to process at the end of a line, bail
		   (when (= depth 0)
		     (return-from iteration-block nil))
		   (if (and (= depth 1) (not current-buffer))
		       (start-buffer)
		       (add-char #\Newline))))
	    (process-brackets-in-line current-line)
	    (move-forward line-iterator)
	    (iterate-elements 
	     line-iterator
	     (lambda (line)
	       (process-brackets-in-line line))))))
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

(defun short-source (source)
  (typecase source
    (pathname source)
    (string 
     (format nil "~a~@[...~]"
	     (substitute-if #\Space
			    (lambda (ch)
			      (or (char= ch #\newline)
				  (char= ch #\linefeed)))
			    (subseq source 0 (min 50 (length source))))
	     (> (length source) 50)))
    (t (format nil "Something of type ~s"
	       (type-of source)))))

(defun could-be-html-tag-p (string start)
  ;; assumes that start == the index _after_ the #\<
  (let ((state :start))
    (loop for index from start below (length string)
       for ch = (schar string index) do
	 (ecase state
	   (:start (when (whitespacep ch) (return nil)) 
		   (setf state :running))
	   (:running 
	    (case ch 
	      (#\' (setf state :single-quote))
	      (#\" (setf state :double-quote))
	      (#\> (return index))))
	   (:single-quote
	    (case ch
	      (#\' (setf state :running))))
	   (:double-quote
	    (case ch
	      (#\" (setf state :running))))))))
	     
;; <a title='>'>
;;  ^

(defun stream-string-for-html (string stream)
  (declare (simple-string string))
  (let ((next-index 1)
	(last-index nil))
    (with-output (out stream)
      (loop for char across string
	 do (cond 
	      ((char= char #\&) (write-string "&amp;" out))
	      ((char= char #\<)
	       (setf last-index (could-be-html-tag-p string next-index))
	       (if last-index
		   (write-char char out)
		   (write-string "&lt;" out)))
	      ((and (null last-index) (char= char #\>))
	       (write-string "&gt;" out))
	      (t (write-char char out)))
	   (when (and last-index (> next-index last-index))
	     (setf last-index nil))
	   (incf next-index)))))

#+(or)
(stream-string-for-html "hello a > b and b < a <a title='>'>" nil)

(defun encode-string-for-html (string)
  (stream-string-for-html string nil))

;; Copied from HTML-Encode
;;?? this is very consy
;;?? crappy name
(defun encode-pre (string)
  (declare (simple-string string))
  (let ((output (make-array (truncate (length string) 2/3)
                            :element-type 'character
                            :adjustable t
                            :fill-pointer 0)))
    (with-output-to-string (out output)
      (loop for char across string
            do (case char
                 ((#\&) (write-string "&amp;" out))
                 ((#\<) (write-string "&lt;" out))
                 ((#\>) (write-string "&gt;" out))
                 (t (write-char char out)))))
    (coerce output 'simple-string)))

;; Copied from HTML-Encode
;;?? this is very consy
;;?? crappy name
;;?? this is really bugging me -- what gross code and it's repeated FOUR times 
;;
(defun encode-string-for-title (string)
  (declare (simple-string string))
  (if (find #\' string :test #'char=)
      (let ((output (make-array (truncate (length string) 2/3)
				:element-type 'character
				:adjustable t
				:fill-pointer 0)))
	(with-output-to-string (out output)
	  (loop for char across string
	     do (case char
		  ;; (code-char 39) ==> #\'
		  ((#\') (write-string "&#39;" out))
		  (t (write-char char out)))))
	(coerce output 'simple-string))
      string))


(defun find-include-file (pathname)
  (bind ((pathname (ensure-string pathname))
	 (search-locations (ensure-list (document-property :search-locations)))
	 (result
	  (or (probe-file (merge-pathnames pathname))
	      ;; look in search-locations
	      (some (lambda (location)
		      (probe-file (merge-pathnames pathname location)))
		    search-locations))))
    (unless result
      (markdown-warning 
       "Unable to find ~a in any of the search-locations ~{~a~^, ~}"
       pathname search-locations))
    result))
  
(defun process-child-markdown (text phase &key (transfer-data nil))
  (bind (((:values child output)
	  (markdown text 
		    :parent *current-document*
		    :format (if (eq phase :parse) :none *current-format*)
		    :properties '((:omit-initial-paragraph t)
				  (:omit-final-paragraph t))
		    :stream nil
		    :document-class 'included-document)))
    (push child (children *current-document*))
    (when transfer-data
      (transfer-link-info *current-document* child "")
      (transfer-document-metadata *current-document* child)
      (transfer-selected-properties 
       *current-document* child
       (set-difference (collect-keys (properties child))
		       (list :omit-initial-paragraph :omit-final-paragraph))))
    (ecase phase
      (:parse child)
      (:render (strip-whitespace output)))))

(defun asdf-system-source-file (system-name)
  (unless (member :asdf *features*)
    (error "Sorry, ASDF is not loaded in this image~%"))
  (let ((system (funcall (read-from-string "asdf:find-system") system-name)))
    (make-pathname 
     :type "asd"
     :name (funcall (read-from-string "asdf:component-name") system)
     :defaults (funcall (read-from-string "asdf:component-relative-pathname") system))))

(defun asdf-system-source-directory (system-name)
  (make-pathname :name nil
                 :type nil
                 :defaults (asdf-system-source-file system-name)))

(defun system-relative-pathname (system pathname &key name type)
  (relative-pathname (asdf-system-source-directory system)
		     pathname :name name :type type))
