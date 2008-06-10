(in-package #:cl-markdown)

(defun markdown (source &key (stream *default-stream*) 
		 (format *default-format*)
		 (additional-extensions nil)
		 (render-extensions nil)
		 (parse-extensions nil)
		 (properties nil)
		 (parent *current-document*)
		 (document-class 'document)
		 )
  "Convert source into a markdown document object and optionally render it to stream using format. Source can be either a string or a pathname or a stream. Stream is like the stream argument in format; it can be a pathname or t \(short for *standard-output*\) or nil \(which will place the output into a string\). Format can be :html or :none. In the latter case, no output will be generated. 

The markdown command returns \(as multiple values\) the generated document object and any return value from the rendering \(e.g., the string produced when the stream is nil\)."
  ;; we chunk-source, run post-processor, handle-spans, cleanup and then render
  (let ((*current-document* (make-container document-class
					    :parent parent
					    :source source))
	(*render-active-functions*
	 (mapcar #'canonize-command
		 (or render-extensions
		     (if additional-extensions
			 `(,@additional-extensions ,@*render-active-functions*)
			 *render-active-functions*))))
	(*parse-active-functions* 
	 (mapcar #'canonize-command
		 (or parse-extensions
		     (if additional-extensions
			 `(,@additional-extensions ,@*parse-active-functions*)
			 *parse-active-functions*))))
	(*default-pathname-defaults* (or (and (typep source 'pathname)
					      (containing-directory source))
					 *default-pathname-defaults*)))
    ;; pull in properties
    (loop for (name . value) in properties do
	 (setf (document-property name) value))
    (chunk-source *current-document* source)
    (iterate-elements 
     (chunk-post-processors *parsing-environment*)
     (lambda (processor)
       (funcall processor *current-document*)))
    (handle-spans *current-document*)
    (cleanup *current-document*)
    (values *current-document* 
            (render-to-stream *current-document* format stream))))

(defun containing-directory (pathspec)
  "Return the containing directory of the thing to which pathspac points. For example:

    (containing-directory \"/foo/bar/bis.temp\")
    \"/foo/bar/\"
    > (containing-directory \"/foo/bar/\")
    \"/foo/\"
"
  (make-pathname
   :directory `(,@(butlast (pathname-directory pathspec)
			   (if (directory-pathname-p pathspec) 1 0)))
   :name nil
   :type nil
   :defaults pathspec))

(defmethod render ((document abstract-document) (style (eql :none)) stream)
  (declare (ignore stream))
  nil)

(defmethod reset ((env parsing-environment))
  (empty! (chunk-parsing-environment env))
  (insert-item (chunk-parsing-environment env)
	       (item-at-1 *chunk-parsing-environments* 'toplevel))
  (setf (chunk-level env) 0
        (current-strip env) "")
  (setf (chunk-post-processors env)
	(list   
	 'handle-link-reference-titles
	 'handle-extended-link-references
         'handle-code                   ; before hr and paragraphs
         'handle-horizontal-rules       ; before bullet lists, after code
         'handle-bullet-lists
         'handle-number-lists
         'handle-paragraphs             ; before headers
	 'handle-bullet-paragraphs
         'handle-blockquotes
         'handle-setext-headers
         'handle-atx-headers
         'merge-chunks-in-document
         'merge-lines-in-chunks
         'canonize-document))
  (empty! (line-code->stripper env))
  (empty! (strippers env))
  (setf (item-at-1 (line-code->stripper env) 'line-is-blockquote-p)
        'blockquote-stripper
        (item-at-1 (line-code->stripper env) 'line-starts-with-bullet-p)
        'one-tab-stripper
        (item-at-1 (line-code->stripper env) 'line-is-code-p)
        'one-tab-stripper
        (item-at-1 (line-code->stripper env) 'line-starts-with-number-p)
        'one-tab-stripper))

(defun null-stripper (line)
  (values line t)) 

(defun one-tab-stripper (line)
  (let ((indentation 0)
        (index 0))
    (loop for ch across line
          while (< indentation *spaces-per-tab*) do
          (incf index)
          (cond ((char= ch #\ )
                 (incf indentation))
                ((char= ch #\Tab)
                 (incf indentation *spaces-per-tab*))
                (t
                 (return))))
    (if (>= indentation *spaces-per-tab*)
      (values (subseq line index) t)
      (values line nil))))
  
;;?? Gary King 2006-01-23: yerch, I don't like it either...
(defun blockquote-stripper (line)
  "Strips putative initial blockquote and up to 3 spaces"
  (let ((count 0)
        (found-bq? nil))
    (cond ((>= (blockquote-count line) 1)
           (loop repeat *spaces-per-tab*
                 for ch across line 
                 while (and (not found-bq?)
                            (or (char-equal ch #\ )
                                (and (char-equal ch #\>)
				     (setf found-bq? t)))) do
                 (incf count))
           (cond ((not (null found-bq?))
                  (when (and (> (size line) (1+ count))
                             (char-equal (aref line count) #\ ))
                    (incf count))
                  (values (subseq line count) t))
                 (t
                  (values line nil))))
          (t
           (values line nil))))) 

(defun blockquote-count (line)
  (let ((count 0))
    (loop for ch across line 
          while (or (char-equal ch #\ )
                    (char-equal ch #\>))
          when (char-equal ch #\>) do (incf count))
    count))

(defun line-indentation (line)
  (let ((count 0))
    (or (loop for ch across line do
              (cond ((char-equal ch #\ ) (incf count))
                    ((char-equal ch #\tab) (incf count *spaces-per-tab*))
                    (t (return count))))
        
        ;; empty line
        (values 0))))

(defun line-changes-indentation-p (line)
  (let ((count 0))
    (loop for ch across line do
          (cond ((char-equal ch #\ ) (incf count))
                ((char-equal ch #\tab) (incf count *spaces-per-tab*))
                (t (return count))))
    (unless (= *current-indentation-level* count)
      (setf *current-indentation-level* count)
      (values t))))
                
(defun line-starts-with-bullet-p (line)
  ;; a bullet and at least one space or tab after it
  (let* ((count 0)
         (bullet? (loop repeat (1- *spaces-per-tab*)
                        for ch across line
                        when (or (char-is-tab-or-space-p ch)
                                 (char-is-bullet-p ch))
                        do (incf count)
                        when (char-is-bullet-p ch) do (return t))))
    (or (and bullet?
             (> (length line) count)
             (char-is-tab-or-space-p (aref line count)))
        (and (not bullet?)
             (> (length line) (1+ count))
             (char-is-bullet-p (aref line count)) 
             (char-is-tab-or-space-p (aref line (1+ count)))))))
             
(defun char-is-tab-or-space-p (ch)
  (or (char-equal ch #\ ) (char-equal ch #\Tab)))

(defun char-is-bullet-p (ch)
  (or (char-equal ch #\*)
      (char-equal ch #\-)
      (char-equal ch #\+)))

(defun line-starts-with-number-p (line)
  ;; at least one digit, then digits and then a period, then a space
  ;; FIXME -- (Why don't I use a regex?)
  (let* ((count 0)
         (number? (loop repeat (1- *spaces-per-tab*)
		     for ch across line
		     when (or (char-is-tab-or-space-p ch)
			      (digit-char-p ch))
		     do (incf count)
		     when (digit-char-p ch) do (return t))))
    (or (and number?
             (> (length line) (1+ count))
             (char-equal (aref line count) #\.)
	     (whitespacep (aref line (1+ count))))
	;; this is in line-starts-with-bullet-p but looks wacked
	#+(or)
        (and (not bullet?)
             (> (length line) (1+ count))
             (char-is-bullet-p (aref line count)) 
             (char-is-tab-or-space-p (aref line (1+ count)))))))

(defun line-is-empty-p (line)
  (every-element-p line #'metatilities:whitespacep))

(defun line-is-not-empty-p (line)
  (not (line-is-empty-p line)))

(defun line-is-blockquote-p (line)
  (unless (line-is-code-p line)
    (let ((trimmed-line (string-left-trim '(#\ ) line)))
      (and (plusp (size trimmed-line))
           (char-equal (aref trimmed-line 0) #\>)))))

(defun line-is-code-p (line)
  (>= (line-indentation line) *spaces-per-tab*))

(defun line-could-be-header-marker-p (line)
  (or (string-starts-with line "------")
      (string-starts-with line "======")))

(defun line-is-link-label-p (line)
  (scan (load-time-value (ppcre:create-scanner '(:sequence link-label))) line))

(defun line-is-extended-link-label-p (line)
  (scan (load-time-value 
	 (ppcre:create-scanner '(:sequence extended-link-label))) line))

(defun line-other-p (line)
  (declare (ignore line))
  ;; catch all
  (values t))

(defun horizontal-rule-char-p (char)
  (member char '(#\- #\* #\_) :test #'char-equal))

(defun line-is-horizontal-rule-p (line)
  (let ((match nil)
        (count 0)
        (possible-hr? nil))
    (loop for char across line do
          (cond ((whitespacep char)
                 ;; ignore
                 )
                
                ((or (and match (char-equal match char))
                     (and (not match) (horizontal-rule-char-p char)))
                 (setf match char)
                 (incf count)
                 (when (>= count *horizontal-rule-count-threshold*)
                   (setf possible-hr? t)))
                (t
                 (setf possible-hr? nil)
                 (return))))
    (values possible-hr?)))
   
(setf (item-at-1 *chunk-parsing-environments* 'toplevel)
      (make-instance
       'chunk-parsing-environment
       :name 'toplevel
       :line-coders '(line-is-empty-p
		      line-is-link-label-p
		      line-is-extended-link-label-p
		      line-is-code-p
		      line-is-blockquote-p
		      line-could-be-header-marker-p
		      line-is-horizontal-rule-p
		      line-starts-with-bullet-p
		      line-starts-with-number-p
		      line-is-not-empty-p
		      line-other)
        :chunk-enders '(line-is-empty-p
                        line-starts-with-number-p
                        line-starts-with-bullet-p
                        line-is-horizontal-rule-p
                        line-is-blockquote-p
                        line-is-link-label-p    ; we'll grab title later...
			line-is-extended-link-label-p
                        line-could-be-header-marker-p
			atx-header-p
                        )
        :chunk-starters '(line-could-be-header-marker-p
			  atx-header-p
                          line-is-not-empty-p )
        :parser-map '((line-is-code-p code))))

(setf (item-at-1 *chunk-parsing-environments* 'line-is-code-p)
      (make-instance 
       'chunk-parsing-environment
       :name 'code
       :chunk-enders '()
       :chunk-starters '()))

(defun maybe-strip-line (line)
  (bind ((env *parsing-environment*)
         (levels 0)
         (stripped? nil)
         ;;?? rather gross, but we don't have reverse iterators yet
         (strippers (reverse (collect-elements (strippers env)))))
    (block stripping
      (iterate-elements 
       strippers
       (lambda (stripper)
         (setf (values line stripped?) (funcall stripper line))
         (unless stripped?
           (return-from stripping))
         (incf levels))))
    (values line levels)))

(defun chunk-source (document source)
  (let* ((result document)
	 (current nil)
	 (current-code nil)
	 (previous-stripper nil)
	 (level 0)
	 (old-level level)
	 (first? 'start-of-document)
	 (was-blank? nil)
	 (been-blank? nil)
	 (just-started? nil)
	 (*default-pathname-defaults* (or (and (typep source 'pathname)
					       (containing-directory source))
					  *default-pathname-defaults*))
	 (line-iterator nil))
    (declare (special line-iterator))
    (reset *parsing-environment*)
    (labels ((code-line (line)
	       (values (some-element-p (line-coders (current-chunk-parser))
				       (lambda (p) (funcall p line)))
		       (some-element-p
			(chunk-enders (current-chunk-parser)) 
			(lambda (p) (funcall p line)))
		       (some-element-p
			(chunk-starters (current-chunk-parser))
			(lambda (p) (funcall p line)))))
	     (end-chunk-p (line code starter ender)
	       ;; End when we have a current chunk AND either
	       ;; the new level is bigger OR there is an ender OR
	       ;; the new level is smaller AND the line isn't empty
	       (declare (ignore starter))
	       (when (and current
			  (or (> level old-level) 
			      (and (< level old-level) 
				   (not (line-is-empty-p line)))
			      ender))
		 ;; special case for hard returns; don't end when the
		 ;; starter is 'line-is-not-empty-p unless it is preceeded
		 ;; by a blank line
		 (and		     ;(not (eq code 'line-is-empty-p))
		  (or (not (eq code 'line-is-not-empty-p))
		      (and (eq code 'line-is-not-empty-p)
			   was-blank?)))))
	     (chunk-line (line)
	       (setf (values line level) (maybe-strip-line line))
	       (unless (line-is-empty-p line)
		 (loop repeat (- old-level level) 
		    while (> (size (chunk-parsing-environment
				    *parsing-environment*)) 1) do
		    (pop-item (chunk-parsing-environment
			       *parsing-environment*))))
	       (bind (((:values code ender starter) (code-line line)))
		 #+(or)
		 (format 
		  t "~%~S~%  L/OL: ~d/~d [~a] ~&  C: ~A E: ~A S: ~A N: ~a X: ~d~@[ L: ~d~] WB: ~a, BB: ~a"
		  line level old-level 
					;(size (strippers *parsing-environment*))
		  (collect-elements (strippers *parsing-environment*))
		  code ender starter (name (current-chunk-parser))
		  (size (chunk-parsing-environment *parsing-environment*))
		  (and current (size (lines current)))
		  was-blank? been-blank?)
		 ;; End current chunk?
		 (when (end-chunk-p line code starter ender)
		   #+(or)
		   (format t " (c: ~a e: ~a) --> end~%" code ender)
		   (setf (ended-by current) code
			 (blank-line-after? current) (line-is-empty-p line))
		   (insert-item (chunks result) current)
		   (setf previous-stripper (stripper? current))
		   (setf current nil))
		 (setf current-code code) 
		 ;; deal with embedded brackets
		 (when (and (not (eq code 'line-is-code-p))
			    (or
			     (not current) 
			     (not (eq (started-by current) 'line-is-code-p))))
		   (setf line (process-brackets document line line-iterator)))
		 ;; Start new chunk?
		 (awhen (and (not current) starter)
		   #+(or)
		   (format t " --> start")
		   (let ((stripper 
			  (item-at-1 (line-code->stripper *parsing-environment*)
				     current-code)))
		     (setf level (+ level (if stripper 1 0))
			   current (make-instance 
				    'chunk 
				    :started-by (or current-code first?)
				    :blank-line-before? (and (not first?) 
							     was-blank?)
				    :indentation (line-indentation line)
				    :level level
				    :stripper? stripper)
			   first? nil
			   (chunk-level *parsing-environment*) level)
		     ;; if there is a new stripper, use it
		     (when stripper
		       (setf line (funcall stripper line)))
		     (when (and (>= level old-level) stripper)
		       (when (and previous-stripper (= level old-level))
			 (pop-item (strippers *parsing-environment*)))
		       (insert-item (strippers *parsing-environment*) stripper)
		       ))
		   (setf just-started? t))
		 ;; add to current chunk
		 (when current
		   (if (line-is-empty-p line)
		       (insert-item (lines current) "")
		       (insert-item (lines current) 
				    ;; consing is fun
				    ;; FIXME - OK, but why do I do this?!
				    ;; A: make sure every line ends with a space.
				    ;; Q: duh, but why?!
				    #+(or)
				    line
				    (if (and (plusp (length line))
					     (char= (aref line (1- (length line))) 
						    #\Space))
					line
					(concatenate 'string line " "))))
		   )
		 (loop while (> level old-level) do
		      (insert-item 
		       (chunk-parsing-environment *parsing-environment*)
		       (or 
			(item-at-1 *chunk-parsing-environments* code)
			(item-at-1 *chunk-parsing-environments* 'toplevel)))
		      (incf old-level))
		 (when (and been-blank?
			    (not (line-is-empty-p line)))
		   (setf old-level level)
		   (loop while (> (size (strippers *parsing-environment*))
				  level) do
			(pop-item (strippers *parsing-environment*))))
		 (setf was-blank? (line-is-empty-p line))
		 (setf been-blank? (unless just-started?
				     (or been-blank? was-blank?)))
		 (setf just-started? nil)
		 )))
      (with-iterator (i source :treat-contents-as :lines
			:skip-empty-chunks? nil)
	(let ((line-iterator i))
	  (declare (special line-iterator))
	  (iterate-elements
	   line-iterator (lambda (line) (chunk-line line))))))
    ;; final processing
    (cond (current
	   ;; Grab last chunk if any
	   (setf (ended-by current) 'end-of-document)
	   (insert-item (chunks result) current))
	  ((not (empty-p (chunks result)))
	   (let ((last (last-item (chunks result))))
	     (setf (blank-line-after? last) t)
	     (when (eq (ended-by last) 'line-is-empty-p)
	       ;; maybe fix ended-by of last chunk
	       (setf (ended-by last) 'end-of-document)))))
    (values result)))

;;; ---------------------------------------------------------------------------
;;; post processors
;;; ---------------------------------------------------------------------------

(defun handle-horizontal-rules (document)
  (iterate-elements
   (chunks document)
   (lambda (chunk)
     (when (or (eq (started-by chunk) 'line-is-horizontal-rule-p)
	       #+(or)
	       ;; no -- case 12
               (eq (ended-by chunk) 'line-is-horizontal-rule-p))
       (empty! (lines chunk))
       (setf (markup-class chunk) '(horizontal-rule))))))

(defmethod it-starts-with-block-level-html-p ((chunk chunk))
  (and (not (empty-p (lines chunk)))
       (it-starts-with-block-level-html-p (first-element (lines chunk)))))

;; FIXME - use an r.e., gosh durn it.
(defmethod it-starts-with-block-level-html-p ((line string))
  (and (> (length line) 2)
       (char= (aref line 0) #\<)
       (let* ((pos-> (position #\> line :test 'char=))
	      (pos-space (position #\Space line :test 'char=))
	      (pos (and pos-> (or (and pos-space (min pos-space pos->)) pos->)))
	      (code (and pos (subseq line 1 pos))))
	 (when (and code (> (length code) 0) (char= (aref code 0) #\/))
	   (setf code (subseq code 1)))
	 (and code (find code *block-level-html-tags*
			 :test 'string-equal)))))

(defun handle-paragraphs (document)
  (let ((first? t))
    (flet ((blank-before-p (chunk)
	     (or (blank-line-before? chunk) 
		 (and 
		  ;;?? probably a hack
		  (or (eq (started-by chunk) 'start-of-document)
		      first?)
		  (not (document-property :omit-initial-paragraph nil)))))
	   (blank-after-p (chunk)
	     (or (blank-line-after? chunk)
		 (and 
		  ;;?? probably a hack
		  (eq (ended-by chunk) 'end-of-document)
		  (not (document-property :omit-final-paragraph nil))))))
      (iterate-elements
       (chunks document)
       (lambda (chunk)
	 (setf (paragraph? chunk)
	       (and 
		(not (it-starts-with-block-level-html-p chunk))
		(not (member 'code (markup-class chunk)))
		(or (and (blank-before-p chunk) 
			 (blank-after-p chunk))
		    (and (or (blank-before-p chunk)
			     (blank-after-p chunk))
			 (not (member (started-by chunk)
				      '(line-starts-with-bullet-p 
					line-starts-with-number-p)))))))
	 (setf first? nil))))))

(defun handle-bullet-paragraphs (document)
  ;; if I have the heuristic right, a list item only gets a paragraph
  ;; if is following (preceeded) by another list item and there is a blank
  ;; line separating them.
  (let ((first? t))
    (labels 
	((blank-before-p (chunk)
	   (or (blank-line-before? chunk) 
	       (and 
		;;?? probably a hack
		(or (eq (started-by chunk) 'start-of-document)
		    first?)
		(not (document-property :omit-initial-paragraph nil)))))
	 (blank-after-p (chunk)
	   (or (blank-line-after? chunk)
	       (and 
		;;?? probably a hack
		(= (level chunk) 1)
		(eq (ended-by chunk) 'end-of-document)
		(not (document-property :omit-final-paragraph nil)))))
	 (list-item-p (chunk)
	   (member (started-by chunk)
		   '(line-starts-with-bullet-p 
		     line-starts-with-number-p)))
	 (handle-triple (a b c)
	   (cond ((and (eq a b) (eq b c)
		       (list-item-p a))
		   ;; all same
		  (setf (paragraph? a) nil))
		 ((not (and (eq a b) (eq b c)))
		  (when (and (list-item-p b))
		    ;; (print (list a b c))
		    (setf (paragraph? b)
			  (or (and (list-item-p a)
				   (= (level a) (level b))
				   (blank-before-p b)
				   :bullet-before)
			      (and (list-item-p c)
				   (= (level b) (level c))
				   (blank-after-p b)
				   :bullet-after))))))
	   (setf first? nil)))
    (map-window-over-elements
     (chunks document) 3 1
     (lambda (triple)
       (bind (((a b c) triple))
	 (handle-triple a b c)))
     :duplicate-ends? t))))

(defun handle-atx-headers (document)
  (iterate-elements
   (chunks document)
   (lambda (chunk)
     (when (and (eq (started-by chunk) 'line-is-not-empty-p)
                (atx-header-p (first-element (lines chunk))))
       (make-header 
	chunk (atx-header-markup-class (first-element (lines chunk)))) 
       (setf (first-element (lines chunk)) 
             (remove-atx-header (first-element (lines chunk))))))))

(defun make-header (chunk markup-class)
  (push markup-class (markup-class chunk))
  (setf (paragraph? chunk) nil))

(defun atx-header-p (line)
  (let ((first-non-hash (position-if 
			 (lambda (ch) (not (char-equal ch #\#))) line)))
    (and first-non-hash
         (< 0 first-non-hash 7))))

(defun atx-header-markup-class (line)
  (let ((first-non-hash (position-if 
			 (lambda (ch) (not (char-equal ch #\#))) line)))
    (case first-non-hash
      (1 'header1)
      (2 'header2)
      (3 'header3)
      (4 'header4)
      (5 'header5)
      (6 'header6)
      (t (error "Unable to determine ATX header class of ~A" line)))))

(defun remove-atx-header (line)
  (string-trim '(#\ ) (string-trim '(#\#) line)))

(defun can-merge-chunks-p (chunk1 chunk2)
  (and (= (level chunk1) (level chunk2))
       (equal (markup-class chunk1) (markup-class chunk2))
       (markup-class-mergable-p (markup-class chunk2))
       (not (paragraph? chunk2))
       (not (eq (ended-by chunk1) 'line-is-empty-p))))

(defmethod markup-class-mergable-p ((markup-class cons))
  (every #'markup-class-mergable-p markup-class))

(defmethod markup-class-mergable-p ((markup-class symbol))
  (member markup-class '(code quote)))

(defun merge-chunks-in-document (document)
  (let ((chunks (make-iterator (chunks document)))
        (gatherer nil))
    (cl-containers::iterate-forward 
     chunks
     (lambda (chunk)
       (if (and gatherer (can-merge-chunks-p gatherer chunk))
         (merge-chunks gatherer chunk)
         (setf gatherer chunk)))))
  (removed-ignored-chunks? document))

(defun merge-chunks (c1 c2)
  (iterate-elements (lines c2) (lambda (l) (insert-item (lines c1) l)))
  (setf (ignore? c2) t))

(defmethod merge-lines-in-chunks ((document abstract-document))
  (iterate-elements
   (chunks document)
   #'merge-lines-in-chunks))

(defmethod merge-lines-in-chunks ((chunk chunk))
  (unless (member 'code (markup-class chunk))
    (setf (slot-value chunk 'lines)
          (merge-lines-in-chunks (lines chunk)))))

(defmethod merge-lines-in-chunks ((lines iteratable-container-mixin))
  (let ((iterator (make-iterator lines))
        (gatherer "")
        (result nil))
    (iterate-forward 
     iterator
     (lambda (line)
       (cond ((can-merge-lines-p gatherer line)
	      (let ((length (length gatherer)))
		(cond ((zerop length)
		       (setf gatherer line))
		      ((char= (aref gatherer (1- length)) #\Space)
		       (setf gatherer (concatenate 'string gatherer line)))
		      (t
		       (setf gatherer 
			     (concatenate 'string gatherer " " line))))))
             (t
              (setf result (append result (list gatherer) (list line)))
              (setf gatherer "")))))
    (when gatherer
      (setf result (append result (list gatherer))))
    result))

(defmethod can-merge-lines-p ((line-1 string) (line-2 string))
  (let ((length-1 (length line-1)))
    (not (and (> length-1 1) 
	     (char= (aref line-1 (- length-1 1)) #\Space) 
	     (char= (aref line-1 (- length-1 2)) #\Space)))))

(defmethod can-merge-lines-p ((line-1 t) (line-2 t))
  (values nil))

(defun handle-setext-headers (document)
  "Find headers chunks that can match up with a previous line and make it so. Also convert line into a header line."
  (map-window-over-elements 
   (chunks document) 2 1
   (lambda (pair)
     (metabang-bind:bind (((p1 p2) pair)) 
       (when (and (not (eq (started-by p1) 'line-is-code-p))
                  (eq (ended-by p1) 'line-could-be-header-marker-p)
                  (eq (started-by p2) 'line-could-be-header-marker-p))
         (make-header p2 (setext-header-markup-class 
			  (first-element (lines p2))))  
         (setf (first-element (lines p2)) (last-element (lines p1)))
         (delete-last (lines p1))
         (when (empty-p (lines p1))
           (setf (ignore? p1) t))))))  
  (removed-ignored-chunks? document))

(defun removed-ignored-chunks? (document)
  (iterate-elements 
   (chunks document) 
   (lambda (chunk)
     (when (ignore? chunk) (delete-item (chunks document) chunk))))
  document)

(defun setext-header-markup-class (line)
  (cond ((char-equal (aref line 0) #\-)
         'header2)
        ((char-equal (aref line 0) #\=)
         'header1)
        (t
         (error "expected a setext header character and got ~A"
		(aref line 0)))))

(defun handle-link-reference-titles (document)
  "Find title lines that can match up with a link reference line 
and make it so. Then parse the links and save them. Finally, remove 
those lines."
  ;; fixup by pulling in titles
  (map-window-over-elements 
   (chunks document) 2 1
   (lambda (pair)
     (bind (((p1 p2) pair)) 
       (when (and (eq (started-by p1) 'line-is-link-label-p)
                  (plusp (size (lines p2)))
                  (line-could-be-link-reference-title-p 
		   (first-element (lines p2))))
         (setf (first-element (lines p1)) 
               (concatenate 'string 
                            (first-element (lines p1)) 
                            (first-element (lines p2)))
               (ended-by p1) 'line-is-link-label-p)
         (delete-first (lines p2))
         (when (empty-p (lines p2))
           (setf (ignore? p2) t))))))
  ;; parse links
  (iterate-elements
   (chunks document)
   (lambda (chunk)
     (when (eq (started-by chunk) 'line-is-link-label-p)
       (bind (((:values nil link-info) 
               (scan-to-strings 'link-label
				(first-element (lines chunk))))
              (id (aref link-info 0))
              (url (aref link-info 1))
              (title (aref link-info 2))
	      (properties (aref link-info 3)))
	 (when title
	   ;; trim off the title delimiters
	   (setf title (subseq title 1 (- (length title) 1))))
         (setf (item-at (link-info document) id)
               (make-instance 'link-info
                 :id id :url url
		 :title title
		 :properties properties)
               (ignore? chunk) t)))))
  ;; now remove the unneeded chunks
  (removed-ignored-chunks? document)
  document)

(defun handle-extended-link-references (document)
  ;; find them and parse them
  (iterate-elements
   (chunks document)
   (lambda (chunk)
     (when (eq (started-by chunk) 'line-is-extended-link-label-p)
       (bind (((:values nil link-info) 
               (scan-to-strings 'extended-link-label 
				(first-element (lines chunk))))
              (id (aref link-info 0))
              (kind (aref link-info 1))
              (contents (aref link-info 2)))
         (setf (item-at (link-info document) id)
               (make-instance 'extended-link-info
                 :id id :kind (form-keyword kind) :contents contents)
               (ignore? chunk) t)))))
  ;; now remove the unneeded chunks
  (removed-ignored-chunks? document)
  document)

(defun line-could-be-link-reference-title-p (line) 
  "True if the first character is a quote after we skip spaces"
  (string-starts-with (strip-whitespace line) "\""))

(defun handle-bullet-lists (document)
  (iterate-elements
   (chunks document)
   (lambda (chunk)
     (when (eq (started-by chunk) 'line-starts-with-bullet-p)
       (push 'bullet (markup-class chunk))
       (setf (first-element (lines chunk)) 
             (remove-bullet (first-element (lines chunk))))))))

(defun remove-bullet (line)
  ;; remove (*|-|+)[.]\s*
  ; assume is a bullet line
  ;;?? possibly a hack -- but expedient!
  (setf line (strip-whitespace line))
  (let ((pos 1)
        (length (size line)))
    (when (and (>= length 2)
               (char-equal (aref line 1) #\.))
      (incf pos 1))
    (loop while (and (> length pos)
                     (whitespacep (aref line pos))) do
          (incf pos))
    (subseq line pos)))
  
(defun handle-number-lists (document)
  (iterate-elements
   (chunks document)
   (lambda (chunk)
     (when (eq (started-by chunk) 'line-starts-with-number-p)
       (push 'number (markup-class chunk))
       (setf (first-element (lines chunk)) 
             (remove-number (first-element (lines chunk))))))))

(defun remove-number (line)
  ;; remove [0-9]*\.\s*
  ; assume is a number line
  (let ((pos 0)
        (length (size line)))
    ;; digits
    (loop while (and (> length pos)
                     (or (digit-char-p (aref line pos))
			 (char-is-tab-or-space-p (aref line pos)))) do
          (incf pos))
    
    ;; required '.'
    (incf pos)
    
    ;; whitespace
    (loop while (and (> length pos)
                     (whitespacep (aref line pos))) do
          (incf pos))
    (subseq line pos)))

(defun handle-blockquotes (document)
  (iterate-elements
   (chunks document)
   (lambda (chunk)
     (when (eq (started-by chunk) 'line-is-blockquote-p)
       (push 'quote (markup-class chunk))))))

(defun remove-blockquote (line)
  ;; removes a single level of blockquoting
  (let ((count 0))
    ;; initial white space
    (loop for ch across line 
          while (whitespacep ch) do (incf count))
    ;; assume #\>
    (incf count)
    (subseq line count)))

(defun handle-code (document)
  (iterate-elements
   (chunks document)
   (lambda (chunk)
     (when (eq (started-by chunk) 'line-is-code-p)
       (push 'code (markup-class chunk))
       (setf (paragraph? chunk) nil)
       ;; remove last line if it is empty
       ;;?? is this really the right place to do this...
       (when (every #'whitespacep (last-item (lines chunk)))
	 (delete-last (lines chunk))
       )))))

(defun remove-indent (line)
  ;; removes a single level of indent
  (let ((count 0)
        (index 0))
    ;; initial white space
    (loop for ch across line 
          when (char-equal ch #\ ) do (incf count)
          when (char-equal ch #\Tab) do (incf count *spaces-per-tab*)
          do (incf index)
          while (< count *spaces-per-tab*))
    (subseq line index)))
  

;;; ---------------------------------------------------------------------------
;;; canonize-document
;;; ---------------------------------------------------------------------------

(defun canonize-document (document)
  (canonize-chunk-markup-class document))

(defun canonize-chunk-markup-class (document)
  (iterate-elements
   (chunks document)
   (lambda (chunk)
     (setf (markup-class chunk)
           (canonize-markup-class chunk)))))

(defun canonize-markup-class (chunk)
  (setf (markup-class chunk)
        (sort (markup-class chunk) #'string-lessp)))

(defun cleanup (document)
  (remove-empty-bits document)
  (handle-paragraph-eval-interactions document)
  (unconvert-escapes document)
  (iterate-elements 
   (item-at-1 (properties document) :cleanup-functions)
   (lambda (fn)
     (funcall fn document))))

(defun remove-empty-bits (document)
  (declare (ignorable document))
  ;; FIXME - if we do this, then the spacing between multiple 
  ;; 'encoded' things (e.g., [link][] `code`) gets lost.
  #+(or)
  (iterate-elements
   (chunks document)
   (lambda (chunk)
     ;;?? yurk -- expediant but ugly
     (unless (member 'code (markup-class chunk))
       (setf (slot-value chunk 'lines) 
	     (collect-elements 
	      (lines chunk)
	      :filter
	      (lambda (line)
		(not (and (stringp line)
			  (zerop 
			   (length (string-trim
				    +whitespace-characters+ line))))))))))))

(defmethod handle-paragraph-eval-interactions ((document abstract-document))
  (iterate-elements (chunks document) #'handle-paragraph-eval-interactions))

(defmethod handle-paragraph-eval-interactions ((chunk chunk))
  (unless (chunk-wants-paragraph-p chunk)
    #+(or)
    (every-element-p 
	 (lines chunk) 
	 (lambda (element)
	   (and (consp element)
		(eq (first element) 'eval)
		(null (second (find (second element) *extensions* 
				    :key 'first))))))
    (setf (paragraph? chunk) nil)))

(defun chunk-wants-paragraph-p (chunk)
  (some-element-p
   (lines chunk) 
   (lambda (line)
     (etypecase line
       (string (find-if (complement #'whitespacep) line))
       (cons (or (not (eq (first line) 'eval))
		 (null (second (find (second line) *extensions* 
				     :key 'first)))))))))

;;; ---------------------------------------------------------------------------
;;; dead code
;;; ---------------------------------------------------------------------------

#+No
;; this one merges only adjencent pairs and screws that up too b/c it merges ignored things...
(defun merge-chunks-in-document (document)
  (map-window-over-elements 
   (chunks document) 2 1
   (lambda (pair)
     (metabang-bind:bind (((c1 c2) pair)) 
       (when (can-merge-chunks-p c1 c2)
         (merge-chunks c1 c2)))))
  (removed-ignored-chunks? document))

