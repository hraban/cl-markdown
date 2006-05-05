(in-package cl-markdown)

(defun markdown (source)
  (let ((document (chunk-source source)))
    (iterate-elements 
     (chunk-post-processors *parsing-environment*)
     (lambda (processor)
       (funcall processor document)))
    (handle-spans document)
    (cleanup document)
    document))

;;; ---------------------------------------------------------------------------

(defmethod reset ((env parsing-environment))
  (setf (chunk-parsing-environment env)
        (item-at-1 *chunk-parsing-environments* 'toplevel))
  (setf (chunk-level env) 0
        (current-strip env) "")
  (setf (chunk-post-processors env)
        (list 'handle-setext-headers
              'handle-link-reference-titles
              'handle-atx-headers
              'handle-code
              'handle-horizontal-rules          ; before bullet lists
              'handle-bullet-lists
              'handle-number-lists
              'handle-paragraphs
              'handle-blockquotes
              'merge-chunks-in-document
              
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
  (let ((indentation (line-indentation line)))
    (if (>= indentation *spaces-per-tab*)
      (values (subseq line *spaces-per-tab*) t)
      (values line nil))))

#+Old
(defun one-tab-stripper (line level)
  "Returns \(as multiple values\) the possibly stripped line and the new level
based on the current level and the number of spaces at the beginning of the line."
  (let* ((looking-for-count (* (1+ level) *spaces-per-tab*))
         (current looking-for-count))
    (loop for ch across line 
          while (char-equal ch #\ ) do
          (when (zerop (decf current))
            (return-from one-tab-stripper (values (subseq line looking-for-count) 
                                                 (1+ level)))))
    (values line 
            (truncate (/ (- looking-for-count current) *spaces-per-tab*)))))

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
  (scan '(:sequence link-label) line))

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
                        )
        :chunk-starters '(line-is-not-empty-p)
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
        (first? 'start-of-document))
    (reset *parsing-environment*)
    (flet ((chunk-line (line)
             ; (format t "~%line: ~S" line)
             
             (setf (values line level) (maybe-strip-line line))
             
             ; (format t "~%~2D ~2D: ~S" level (size (strippers *parsing-environment*)) line)
             
             (let ((code (some-element-p (line-coders (current-chunk-parser))
                                         (lambda (p) (funcall p line)))))
               ;; End current chunk?
               (when (or (/= level old-level) 
                         ;(not (eq current-code code))
                         (and current
                              (some-element-p (chunk-enders (current-chunk-parser)) 
                                              (lambda (p) (funcall p line)))))
                 (when current 
                   (setf (ended-by current) code)
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
     (when (or (eq (started-by chunk) 'start-of-document)
               (eq (started-by chunk) 'line-is-empty-p)
               (eq (ended-by chunk) 'line-is-empty-p)
               (eq (ended-by chunk) 'end-of-document))
       (setf (paragraph? chunk) t)))))

;;; ---------------------------------------------------------------------------
  
(defun handle-atx-headers (document)
  (iterate-elements
   (chunks document)
   (lambda (chunk)
     (when (atx-header-p (first-element (lines chunk)))
       (push (atx-header-markup-class (first-element (lines chunk))) 
             (markup-class chunk))
       (setf (first-element (lines chunk)) 
             (remove-atx-header (first-element (lines chunk))))))))

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

(defun can-merge-p (chunk1 chunk2)
  (and (= (level chunk1) (level chunk2))
       (equal (markup-class chunk1) (markup-class chunk2))
       (not (paragraph? chunk2))
       (not (eq (ended-by chunk1) 'line-is-empty-p))))

;;; ---------------------------------------------------------------------------

(defun merge-chunks-in-document (document)
  (let ((chunks (make-iterator (chunks document)))
        (gatherer nil))
    (cl-containers::iterate-forward 
     chunks
     (lambda (chunk)
       (if (and gatherer (can-merge-p gatherer chunk))
         (merge-chunks gatherer chunk)
         (setf gatherer chunk)))))
  (removed-ignored-chunks? document))

;;; ---------------------------------------------------------------------------

(defun merge-chunks (c1 c2)
  (iterate-elements (lines c2) (lambda (l) (insert-item (lines c1) l)))
  (setf (ignore? c2) t))
  
;;; ---------------------------------------------------------------------------

(defun handle-setext-headers (document)
  "Find headers chunks that can match up with a previous line and make it so. Also convert line into a header line."
  (map-window-over-elements 
   (chunks document) 2 1
   (lambda (pair)
     (metabang-bind:bind (((p1 p2) pair)) 
       (when (and (eq (ended-by p1) 'line-could-be-header-marker-p)
                  (eq (started-by p2) 'line-could-be-header-marker-p))
         (push (setext-header-markup-class (first-element (lines p2))) 
               (markup-class p2))  
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
       #+No
       (setf (first-element (lines chunk)) 
             (remove-indent (first-element (lines chunk))))))))

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
  (iterate-elements
   (chunks document)
   (lambda (chunk)
     ;;?? yurk -- expediant but ugly
     (setf (slot-value chunk 'lines) 
           (remove-if 
            (lambda (line)
              (and (stringp line) (string-equal line "")))
            (lines chunk))))))



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
       (when (can-merge-p c1 c2)
         (merge-chunks c1 c2)))))
  (removed-ignored-chunks? document))

