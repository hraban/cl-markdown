(in-package #:cl-markdown)

#|
;; bug
(markdown
 "
# A
# B
")

(let ((*parse-active-functions* '(set-property))
      (*render-active-functions* '(table-of-contents property)))
  (markdown
   "
{set-property HTML t}
{set-property title My best summer vacation ever}

{set-property style-sheet my-style.css}
{set-property Author Gary W. King} 
{table-of-contents :depth 2}

# B

## B1

### B1A

## B2

# C

Written by {property Author} on {modification-date}.
"
   :format :html))

(markdown
 (make-pathname :type "text"
                :name "Markdown Documentation - Basics"  
                :defaults cl-markdown-test::*test-source-directory*)
 :format :none)

(in-package #:cl-markdown)



(let ((*output-stream* t))
  (iterate-elements
   (let ((*current-document* ccl:!))
     (generate-table-of-contents :depth 2))
   #'render-to-html))

{modified-date}
{render-date}

(with-new-file (s "ccl:tmp.tmp")
  (file-write-date s))

(let ((s (make-string-input-stream "hello")))
  (file-write-date s))

(deftestsuite insert-item-at-test ()
  ())

(addtest (insert-item-at-test)
  basic-insert
  (let ((c (make-container 'vector-container 
                                   :initial-contents '(1 3 4 5))))
    (insert-item-at c 2 1)
    (ensure-same (item-at c 1) 2)
    (ensure-same (item-at c 0) 1)
    (ensure-same (item-at c 2) 3)))

#+(or)
;; fails - infinite runs
(addtest (insert-item-at-test)
  insert-while-iterating
  (let ((c (make-container 'vector-container 
                                   :initial-contents '(1 2 4 5))))
    ;;?? correctly runs forever
    #+(or)
    (iterate-elements
     c (lambda (e)
         (when (= e 2)
           (insert-item-at c 3 2))))    
    (iterate-elements
     c (lambda (e)
         (print e)
         (when (= e 2)
           (insert-item-at c 3 2))))
    (ensure-same (collect-elements c) '(1 2 3 4 5) :test 'equal)))

#+(or)
;; fails - infinite loop
(addtest (insert-item-at-test)
  insert-while-iterating
  (let* ((c (make-container 'vector-container 
                            :initial-contents '(1 2 4 5)))
         (i (make-iterator c)))
    (iterate-elements i (lambda (e)
                          (when (= e 4)         ;<---- no good
                            (insert-item-at c 3 2))))
    (ensure-same (collect-elements c) '(1 2 3 4 5) :test 'equal)))


(addtest (insert-item-at-test)
  insert-while-iterating
  (let* ((c (make-container 'vector-container 
                            :initial-contents '(1 2 4 5)))
         (i (make-iterator c)))
    (iterate-elements i (lambda (e)
                          (when (= e 2)
                            (insert-item-at c 3 2))))
    (ensure-same (collect-elements c) '(1 2 3 4 5) :test 'equal)))

|#

(defun markdown (source &key (stream *default-stream*) (format *default-format*))
  "Convert source into a markdown document object and optionally render it to stream using format. Source can be either a string or a pathname or a stream. Stream is like the stream argument in format; it can be a pathname or t \(short for *standard-output*\) or nil \(which will place the output into a string\). Format can be :html or :none. In the latter case, no output will be generated. 

The markdown command returns \(as multiple values\) the generated document object and any return value from the rendering \(e.g., the string produced when the stream is nil\)."
  ;; we chunk-source, run post-processor, handle-spans, cleanup and then render
  (let ((*current-document* (chunk-source source)))
    (iterate-elements 
     (chunk-post-processors *parsing-environment*)
     (lambda (processor)
       (funcall processor *current-document*)))
    (handle-spans *current-document*)
    (cleanup *current-document*)
    (values *current-document* 
            (render-to-stream *current-document* format stream))))

;;; ---------------------------------------------------------------------------

(defmethod render ((document document) (style (eql :none)) stream)
  nil)

;;; ---------------------------------------------------------------------------

(defmethod reset ((env parsing-environment))
  (setf (chunk-parsing-environment env)
        (item-at-1 *chunk-parsing-environments* 'toplevel))
  (setf (chunk-level env) 0
        (current-strip env) "")
  (setf (chunk-post-processors env)
        (list               
         'handle-link-reference-titles
         'handle-code                   ; before hr and paragraphs
         'handle-horizontal-rules       ; before bullet lists, after code
         'handle-bullet-lists
         'handle-number-lists
         'handle-paragraphs             ; before headers
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

;;; ---------------------------------------------------------------------------

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
  
#+Old
(defun one-tab-stripper (line)
  (let ((indentation (line-indentation line)))
    (if (>= indentation *spaces-per-tab*)
      (values (subseq line *spaces-per-tab*) t)
      (values line nil))))

;;; ---------------------------------------------------------------------------

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
                                (and (char-equal ch #\>) (setf found-bq? t)))) do
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

;;; ---------------------------------------------------------------------------

(defun blockquote-count (line)
  (let ((count 0))
    (loop for ch across line 
          while (or (char-equal ch #\ )
                    (char-equal ch #\>))
          when (char-equal ch #\>) do (incf count))
    count))
  
;;; --------------------------------------------------------------------------- 

(defun line-indentation (line)
  (let ((count 0))
    (or (loop for ch across line do
              (cond ((char-equal ch #\ ) (incf count))
                    ((char-equal ch #\tab) (incf count *spaces-per-tab*))
                    (t (return count))))
        
        ;; empty line
        (values 0))))

;;; ---------------------------------------------------------------------------

(defun line-changes-indentation-p (line)
  (let ((count 0))
    (loop for ch across line do
          (cond ((char-equal ch #\ ) (incf count))
                ((char-equal ch #\tab) (incf count *spaces-per-tab*))
                (t (return count))))
    (unless (= *current-indentation-level* count)
      (setf *current-indentation-level* count)
      (values t))))

;;; ---------------------------------------------------------------------------
                
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

;;; ---------------------------------------------------------------------------
             
(defun char-is-tab-or-space-p (ch)
  (or (char-equal ch #\ ) (char-equal ch #\Tab)))

;;; ---------------------------------------------------------------------------

(defun char-is-bullet-p (ch)
  (or (char-equal ch #\*)
      (char-equal ch #\-)
      (char-equal ch #\+)))
  
;;; ---------------------------------------------------------------------------

(defun line-starts-with-number-p (line)
  ;; at least one digit, then digits and then a period
  (and (length-at-least-p line 2)
       (digit-char-p (aref line 0))
        (char-equal 
         (aref line (position-if (complement #'digit-char-p) line)) #\.)))

;;; ---------------------------------------------------------------------------

(defun line-is-empty-p (line)
  (every-element-p line #'metatilities:whitespacep))

;;; ---------------------------------------------------------------------------

(defun line-is-not-empty-p (line)
  (not (line-is-empty-p line)))

;;; ---------------------------------------------------------------------------

(defun line-is-blockquote-p (line)
  (unless (line-is-code-p line)
    (let ((trimmed-line (string-left-trim '(#\ ) line)))
      (and (plusp (size trimmed-line))
           (char-equal (aref trimmed-line 0) #\>)))))

;;; ---------------------------------------------------------------------------

(defun line-is-code-p (line)
  (>= (line-indentation line) *spaces-per-tab*))

;;; ---------------------------------------------------------------------------

(defun line-could-be-header-marker-p (line)
  (or (string-starts-with line "------")
      (string-starts-with line "======")))

;;; ---------------------------------------------------------------------------

(defun line-is-link-label-p (line)
  (scan (ppcre:create-scanner '(:sequence link-label)) line))

;;; ---------------------------------------------------------------------------

(defun line-other-p (line)
  (declare (ignore line))
  ;; catch all
  (values t))

;;; ---------------------------------------------------------------------------

(defun horizontal-rule-char-p (char)
  (member char '(#\- #\* #\_) :test #'char-equal))

;;; ---------------------------------------------------------------------------

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
   
;;; ---------------------------------------------------------------------------

(setf (item-at-1 *chunk-parsing-environments* 'toplevel)
      (make-instance 'chunk-parsing-environment
        :line-coders '(line-is-empty-p
                       line-is-link-label-p
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
                        line-could-be-header-marker-p
                        )
        :chunk-starters '(line-could-be-header-marker-p
                          line-is-not-empty-p )
        :parser-map '((line-starts-with-bullet-p bullets))))

;;; ---------------------------------------------------------------------------

(setf (item-at-1 *chunk-parsing-environments* 'bullets)
      (make-instance 'chunk-parsing-environment
        :chunk-enders '(line-is-empty-p
                        line-could-be-header-marker-p
                        line-starts-with-bullet-p)
        :chunk-starters '(line-is-not-empty-p)))

;;; ---------------------------------------------------------------------------

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

;;; ---------------------------------------------------------------------------

(defun chunk-source (source)
  (let* ((result (make-container 'document))
        (current nil)
        (current-code nil)
        (level 0)
        (old-level level)
        (first? 'start-of-document)
        (was-blank? nil))
    (reset *parsing-environment*)
    (flet ((chunk-line (line)
             ; (format t "~%line: ~S" line)
             (setf (values line level) (maybe-strip-line line))
             
             ; (format t "~%~2D ~2D: ~S" level (size (strippers *parsing-environment*)) line)
             #+Ignore
             (format t "~%~A: ~A ~A ~A"
                     line
                     (some-element-p (line-coders (current-chunk-parser))
                                     (lambda (p) (funcall p line)))
                     (some-element-p (chunk-enders (current-chunk-parser)) 
                                     (lambda (p) (funcall p line)))
                     (some-element-p (chunk-starters (current-chunk-parser))
                                     (lambda (p) (funcall p line))))
             
             (let ((code (some-element-p (line-coders (current-chunk-parser))
                                         (lambda (p) (funcall p line)))))
               ;; End current chunk?
               (when (or (/= level old-level) 
                         ;(not (eq current-code code))
                         (and current
                              (some-element-p (chunk-enders (current-chunk-parser)) 
                                              (lambda (p) (funcall p line)))))
                 (when current 
                   (setf (ended-by current) code
                         (blank-line-after? current) (line-is-empty-p line))
                   (insert-item (chunks result) current)
                   (setf current nil)))
               (setf current-code code) 
               
               ;; Start new chunk?
               (awhen (and (not current)
                           (some-element-p (chunk-starters (current-chunk-parser))
                                           (lambda (p) (funcall p line))))
                 (let ((stripper (item-at-1 (line-code->stripper *parsing-environment*)
                                            current-code)))
                   (setf level (+ level (if stripper 1 0))
                         current (make-instance 'chunk 
                                   :started-by (or current-code first?)
                                   :blank-line-before? was-blank?
                                   :indentation (line-indentation line)
                                   :level level)
                         first? nil
                         (chunk-level *parsing-environment*) level)
                   
                   ;; if there is a new stripper, use it
                   (when stripper
                     (setf line (funcall stripper line)))
                   
                   (when (and (>= level old-level) stripper)
                     (insert-item (strippers *parsing-environment*) stripper)
                     ; (format t " -~2D-" (size (strippers *parsing-environment*)))
                     )))
               
               (setf was-blank? (line-is-empty-p line))
               
               (loop while (> (size (strippers *parsing-environment*)) level) do
                     ; (princ " -XX-")
                     (pop-item (strippers *parsing-environment*)))
               ;; add to current chunk
               ; (format t "~%    ~S: ~S" code line)
               (when current
                 (insert-item (lines current) line))
               (setf old-level level))))
      (with-iterator (i source :treat-contents-as :lines :skip-empty-chunks? nil)
        (iterate-elements
         i
         (lambda (line)
           (chunk-line line)))))
      
    ;; Grab last chunk if any
    (when current
      (setf (ended-by current) 'end-of-document)
      (insert-item (chunks result) current))
    
    (values result)))

;;; ---------------------------------------------------------------------------
;;; post processors
;;; ---------------------------------------------------------------------------

(defun handle-horizontal-rules (document)
  (iterate-elements
   (chunks document)
   (lambda (chunk)
     (when (or (eq (started-by chunk) 'line-is-horizontal-rule-p)
               (eq (ended-by chunk) 'line-is-horizontal-rule-p))
       (empty! (lines chunk))
       (setf (markup-class chunk) '(horizontal-rule))))))

;;; ---------------------------------------------------------------------------

(defun handle-paragraphs (document)
  (iterate-elements
   (chunks document)
   (lambda (chunk)
     (when (or (and (blank-line-before? chunk) 
                    (blank-line-after? chunk)
                    (not (member 'code (markup-class chunk))))
               (and (or (blank-line-before? chunk) 
                        (blank-line-after? chunk)
                        (eq (started-by chunk) 'start-of-document)
                        (eq (ended-by chunk) 'end-of-document))
                    (not (member (started-by chunk)
                                 '(line-starts-with-bullet-p 
                                   line-starts-with-number-p)))
                    (not (member 'code (markup-class chunk)))))
       (setf (paragraph? chunk) t)))))

;;; ---------------------------------------------------------------------------
  
(defun handle-atx-headers (document)
  (iterate-elements
   (chunks document)
   (lambda (chunk)
     (when (and (eq (started-by chunk) 'line-is-not-empty-p)
                (atx-header-p (first-element (lines chunk))))
       (make-header chunk (atx-header-markup-class (first-element (lines chunk)))) 
       (setf (first-element (lines chunk)) 
             (remove-atx-header (first-element (lines chunk))))))))

;;; ---------------------------------------------------------------------------

(defun make-header (chunk markup-class)
  (push markup-class (markup-class chunk))
  (setf (paragraph? chunk) nil))

;;; ---------------------------------------------------------------------------

(defun atx-header-p (line)
  (let ((first-non-hash (position-if (lambda (ch) (not (char-equal ch #\#))) line)))
    (and first-non-hash
         (< 0 first-non-hash 7))))

;;; ---------------------------------------------------------------------------

(defun atx-header-markup-class (line)
  (let ((first-non-hash (position-if (lambda (ch) (not (char-equal ch #\#))) line)))
    (case first-non-hash
      (1 'header1)
      (2 'header2)
      (3 'header3)
      (4 'header4)
      (5 'header5)
      (6 'header6)
      (t (error "Unable to determine ATX header class of ~A" line)))))

;;; ---------------------------------------------------------------------------

(defun remove-atx-header (line)
  (string-trim '(#\ ) (string-trim '(#\#) line)))

;;; ---------------------------------------------------------------------------

(defun can-merge-chunks-p (chunk1 chunk2)
  (and (= (level chunk1) (level chunk2))
       (equal (markup-class chunk1) (markup-class chunk2))
       (markup-class-mergable-p (markup-class chunk2))
       (not (paragraph? chunk2))
       (not (eq (ended-by chunk1) 'line-is-empty-p))))

;;; ---------------------------------------------------------------------------

(defmethod markup-class-mergable-p ((markup-class cons))
  (every #'markup-class-mergable-p markup-class))

;;; ---------------------------------------------------------------------------

(defmethod markup-class-mergable-p ((markup-class symbol))
  (member markup-class '(code quote)))

;;; ---------------------------------------------------------------------------

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

;;; ---------------------------------------------------------------------------

(defun merge-chunks (c1 c2)
  (iterate-elements (lines c2) (lambda (l) (insert-item (lines c1) l)))
  (setf (ignore? c2) t))

;;; ---------------------------------------------------------------------------

(defmethod merge-lines-in-chunks ((document document))
  (iterate-elements
   (chunks document)
   #'merge-lines-in-chunks))

;;; ---------------------------------------------------------------------------

(defmethod merge-lines-in-chunks ((chunk chunk))
  (unless (member 'code (markup-class chunk))
    (setf (slot-value chunk 'lines)
          (merge-lines-in-chunks (lines chunk)))))

;;; ---------------------------------------------------------------------------

(defmethod merge-lines-in-chunks ((lines iteratable-container-mixin))
  (let ((iterator (make-iterator lines))
        (gatherer "")
        (result nil))
    (iterate-forward 
     iterator
     (lambda (line)
       (cond ((can-merge-lines-p gatherer line)
              (setf gatherer (concatenate 'string gatherer line " ")))
             (t
              (setf result (append result (list gatherer) (list line)))
              (setf gatherer "")))))
    
    (when gatherer
      (setf result (append result (list gatherer))))
    result))

;;; ---------------------------------------------------------------------------

(defmethod can-merge-lines-p ((line-1 string) (line-2 string))
  (values t))

;;; ---------------------------------------------------------------------------

(defmethod can-merge-lines-p ((line-1 t) (line-2 t))
  (values nil))

;;; ---------------------------------------------------------------------------

(defun handle-setext-headers (document)
  "Find headers chunks that can match up with a previous line and make it so. Also convert line into a header line."
  (map-window-over-elements 
   (chunks document) 2 1
   (lambda (pair)
     (metabang-bind:bind (((p1 p2) pair)) 
       (when (and (not (eq (started-by p1) 'line-is-code-p))
                  (eq (ended-by p1) 'line-could-be-header-marker-p)
                  (eq (started-by p2) 'line-could-be-header-marker-p))
         (make-header p2 (setext-header-markup-class (first-element (lines p2))))  
         (setf (first-element (lines p2)) (last-element (lines p1)))
         (delete-last (lines p1))
         (when (empty-p (lines p1))
           (setf (ignore? p1) t))))))
  
  (removed-ignored-chunks? document))

;;; ---------------------------------------------------------------------------

(defun removed-ignored-chunks? (document)
  (iterate-elements 
   (chunks document) 
   (lambda (chunk)
     (when (ignore? chunk) (delete-item (chunks document) chunk))))
  document)

;;; ---------------------------------------------------------------------------

(defun setext-header-markup-class (line)
  (cond ((char-equal (aref line 0) #\-)
         'header2)
        ((char-equal (aref line 0) #\=)
         'header1)
        (t
         (error "expected a setext header character and got ~A" (aref line 0)))))

;;; ---------------------------------------------------------------------------

(defun handle-link-reference-titles (document)
  "Find title lines that can match up with a link reference line and make it so. 
Then parse the links and save them. Finally, remove those lines."
  ;; fixup by pulling in titles
  (map-window-over-elements 
   (chunks document) 2 1
   (lambda (pair)
     (bind (((p1 p2) pair)) 
       (when (and (eq (started-by p1) 'line-is-link-label-p)
                  (plusp (size (lines p2)))
                  (line-could-be-link-reference-title-p (first-element (lines p2))))
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
       (bind (((values nil link-info) 
               (scan-to-strings '(:sequence link-label) (first-element (lines chunk))))
              (id (aref link-info 0))
              (url (aref link-info 1))
              (title (aref link-info 2)))
         (setf (item-at (link-info document) id)
               (make-instance 'link-info
                 :id id :url url :title title)
               (ignore? chunk) t)))))
  
  ;; now remove the unneeded chunks
  (iterate-elements 
   (chunks document) 
   (lambda (chunk)
     (when (ignore? chunk) (delete-item (chunks document) chunk))))
  document)

;;; ---------------------------------------------------------------------------
  
(defun line-could-be-link-reference-title-p (line) 
  "True if the first character is a quote after we skip spaces"
  (string-starts-with (string-left-trim '(#\ ) line) "\""))
  
;;; ---------------------------------------------------------------------------

(defun handle-bullet-lists (document)
  (iterate-elements
   (chunks document)
   (lambda (chunk)
     (when (eq (started-by chunk) 'line-starts-with-bullet-p)
       (push 'bullet (markup-class chunk))
       (setf (first-element (lines chunk)) 
             (remove-bullet (first-element (lines chunk))))))))

;;; ---------------------------------------------------------------------------

(defun remove-bullet (line)
  ;; remove (*|-|+)[.]\s*
  ; assume is a bullet line
  (let ((pos 1)
        (length (size line)))
    (when (and (>= length 2)
               (char-equal (aref line 1) #\.))
      (incf pos 1))
    (loop while (and (>= length (1+ pos))
                     (whitespacep (aref line pos))) do
          (incf pos))
    (subseq line pos)))
  
;;; ---------------------------------------------------------------------------

(defun handle-number-lists (document)
  (iterate-elements
   (chunks document)
   (lambda (chunk)
     (when (eq (started-by chunk) 'line-starts-with-number-p)
       (push 'number (markup-class chunk))
       (setf (first-element (lines chunk)) 
             (remove-number (first-element (lines chunk))))))))

;;; ---------------------------------------------------------------------------

(defun remove-number (line)
  ;; remove [0-9]*\.\s*
  ; assume is a number line
  (let ((pos 0)
        (length (size line)))
    ;; digits
    (loop while (and (> length pos)
                     (digit-char-p (aref line pos))) do
          (incf pos))
    
    ;; required '.'
    (incf pos)
    
    ;; whitespace
    (loop while (and (> length pos)
                     (whitespacep (aref line pos))) do
          (incf pos))
    (subseq line pos)))

;;; ---------------------------------------------------------------------------

(defun handle-blockquotes (document)
  (iterate-elements
   (chunks document)
   (lambda (chunk)
     (when (eq (started-by chunk) 'line-is-blockquote-p)
       (push 'quote (markup-class chunk))
       
       #+No
       (setf (first-element (lines chunk)) 
             (remove-blockquote (first-element (lines chunk))))))))

;;; ---------------------------------------------------------------------------

(defun remove-blockquote (line)
  ;; removes a single level of blockquoting
  (let ((count 0))
    ;; initial white space
    (loop for ch across line 
          while (whitespacep ch) do (incf count))
    ;; assume #\>
    (incf count)
    (subseq line count)))

;;; ---------------------------------------------------------------------------

(defun handle-code (document)
  (iterate-elements
   (chunks document)
   (lambda (chunk)
     (when (eq (started-by chunk) 'line-is-code-p)
       (push 'code (markup-class chunk))
       (setf (paragraph? chunk) nil)))))

;;; ---------------------------------------------------------------------------

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

;;; ---------------------------------------------------------------------------

(defun canonize-chunk-markup-class (document)
  (iterate-elements
   (chunks document)
   (lambda (chunk)
     (setf (markup-class chunk)
           (canonize-markup-class chunk)))))

;;; ---------------------------------------------------------------------------

(defun canonize-markup-class (chunk)
  (setf (markup-class chunk)
        (sort (markup-class chunk) #'string-lessp)))

;;; ---------------------------------------------------------------------------

(defun cleanup (document)
  (remove-empty-bits document)
  (iterate-elements 
   (item-at-1 (properties document) :cleanup-functions)
   (lambda (fn)
     (funcall fn document))))

;;; ---------------------------------------------------------------------------

(defun remove-empty-bits (document)
  (iterate-elements
   (chunks document)
   (lambda (chunk)
     ;;?? yurk -- expediant but ugly
     (setf (slot-value chunk 'lines) 
           (collect-elements 
            (lines chunk)
            :filter
            (lambda (line)
              (not (and (stringp line) 
                        (zerop 
                         (length (string-trim +whitespace-characters+ line)))))))))))

+whitespace-characters+

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

