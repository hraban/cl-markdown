(in-package cl-markdown)

(defun markdown (source)
  (let ((document (chunk-source source)))
    (iterate-elements 
     (chunk-post-processors *parsing-environment*)
     (lambda (processor)
       (funcall processor document)))
    document))

;;; ---------------------------------------------------------------------------

(defmethod reset ((env parsing-environment))
  (setf (chunk-parsing-environment env)
        (item-at-1 *chunk-parsing-environments* 'toplevel))
  (setf (chunk-post-processors env)
        (list 'handle-setext-headers
              'handle-atx-headers
              'handle-bullet-lists
              'handle-number-lists
              'handle-paragraphs
              
              'canonize-document)))
                                  
(defun line-indentation (line)
  (let ((count 0))
    (loop for ch across line do
          (cond ((char-equal ch #\ ) (incf count))
                ((char-equal ch #\tab) (incf count *spaces-per-tab*))
                (t (return count))))))

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
  (or (string-starts-with line "*")
      (string-starts-with line "-")
      (string-starts-with line "+")))

(defun line-starts-with-number-p (line)
  ;; at least one digit, then digits and then a period
  (and (length-at-least-p line 2)
       (digit-char-p (aref line 0))
        (char-equal 
         (aref line (position-if (complement #'digit-char-p) line)) #\.)))

(defun line-is-empty-p (line)
  (every-element-p line #'metatilities:whitespacep))

(defun line-is-not-empty-p (line)
  (not (line-is-empty-p line)))

(defun line-could-be-header-marker-p (line)
  (or (string-starts-with line "------")
      (string-starts-with line "======")))

(defun line-other-p (line)
  (declare (ignore line))
  ;; catch all
  (values t))

;;; ---------------------------------------------------------------------------

(setf (item-at-1 *chunk-parsing-environments* 'toplevel)
      (make-instance 'chunk-parsing-environment
        :line-coders '(line-is-empty-p
                       line-could-be-header-marker-p
                       line-starts-with-bullet-p
                       line-starts-with-number-p
                       line-is-not-empty-p
                       line-other)
        :chunk-enders '(line-is-empty-p
                        line-starts-with-number-p
                        line-starts-with-bullet-p)
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

(defun chunk-source (source)
  (let ((result (make-container 'document))
        (current nil)
        (line-code nil)
        (first? 'start-of-document))
    (reset *parsing-environment*)
    (iterate-elements
     (make-iterator source :treat-contents-as :lines :skip-empty-chunks? nil)
     (lambda (line)
       (let ((code (some-element-p (line-coders (current-chunk-parser))
                                   (lambda (p) (funcall p line)))))
         (when (or (not (eq line-code code))
                   (and current
                        (some-element-p (chunk-enders (current-chunk-parser)) 
                                        (lambda (p) (funcall p line)))))
           (when current 
             (setf (ended-by current) code)
             (insert-item (chunks result) current)
             (setf current nil))
           (setf line-code code))
         
         (awhen (and (not current)
                     (some-element-p (chunk-starters (current-chunk-parser))
                                     (lambda (p) (funcall p line))))
           (setf current (make-instance 'chunk 
                           :started-by (or first? line-code)
                           :indentation (line-indentation line))
                 first? nil)))
       
       (when current
         (insert-item (lines current) line))
       
       #+Ignore
       (awhen (find code (parser-map (current-chunk-parser)))
         )))
    
    (when current
      (setf (ended-by current) 'end-of-document)
      (insert-item (chunks result) current))
    
    (values result)))

;;; ---------------------------------------------------------------------------
;;; post processors
;;; ---------------------------------------------------------------------------

(defun handle-paragraphs (document)
  (iterate-elements
   (chunks document)
   (lambda (chunk)
     (when (or (eq (started-by chunk) 'start-of-document)
               (eq (started-by chunk) 'line-is-empty-p)
               (eq (ended-by chunk) 'line-is-empty-p)
               (eq (ended-by chunk) 'end-of-document))
       (push 'paragraph (markup-classes chunk))))))

;;; ---------------------------------------------------------------------------
  
(defun handle-atx-headers (document)
  (iterate-elements
   (chunks document)
   (lambda (chunk)
     (when (atx-header-p (first-element (lines chunk)))
       (push (atx-header-markup-class (first-element (lines chunk))) 
             (markup-classes chunk))
       (setf (first-element (lines chunk)) 
             (remove-atx-header (first-element (lines chunk))))))))

;;; ---------------------------------------------------------------------------

(defun atx-header-p (line)
  (let ((first-non-hash (position-if (lambda (ch) (not (char-equal ch #\#))) line)))
    (< 0 first-non-hash 7)))

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

(defun handle-setext-headers (document)
  "Find headers chunks that can match up with a previous line and make it so. Also convert line into a header line."
  (map-window-over-elements 
   (chunks document) 2 1
   (lambda (pair)
     (metabang-bind:bind (((p1 p2) pair)) 
       (when (and (eq (ended-by p1) 'line-could-be-header-marker-p)
                  (eq (started-by p2) 'line-could-be-header-marker-p))
         (push (setext-header-markup-class (first-element (lines p2))) 
               (markup-classes p2))  
         (setf (first-element (lines p2)) (last-element (lines p1)))
         (delete-last (lines p1))
         (when (empty-p (lines p1))
           (setf (ignore? p1) t))))))
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

(defun handle-bullet-lists (document)
  (iterate-elements
   (chunks document)
   (lambda (chunk)
     (when (eq (started-by chunk) 'line-starts-with-bullet-p)
       (push 'bullet (markup-classes chunk))
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
;;; canonize-document
;;; ---------------------------------------------------------------------------

(defun canonize-document (document)
  (canonize-chunk-markup-classes document))

;;; ---------------------------------------------------------------------------

(defun canonize-chunk-markup-classes (document)
  (iterate-elements
   (chunks document)
   (lambda (chunk)
     (setf (markup-classes chunk)
           (canonize-markup-classes chunk)))))

;;; ---------------------------------------------------------------------------

(defun canonize-markup-classes (chunk)
  (setf (markup-classes chunk)
        (sort (markup-classes chunk) #'string-lessp)))