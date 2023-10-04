(in-package #:cl-markdown)

#|
(markdown "`test` blue **beans**" :format :plain)
(markdown "Eta

 * beta
 * data

Eta" :format :plain)
|#

(defmethod render ((document abstract-document) (style (eql :plain)) stream)
  (declare (ignore stream))
  (render-plain document))

(defmethod render-plain ((document abstract-document))
  (bind ((current-chunk nil))
    (labels ((render-block (block level markup inner?)
	       (declare (ignore markup))
	       (let ((add-markup? (not (eq (first block) current-chunk))))
		 (cond ((or (length-1-list-p block))
			(render-plain (first block)))
		       ((not add-markup?)
			(render-plain (first block))
			(do-it (rest block) level))
		       (t
			(setf current-chunk (and inner? (first block)))
			(do-it block level)))))
	     (do-it (chunks level)
	       (loop for rest = chunks then (rest rest)
		  for chunk = (first rest) then (first rest)
		  while chunk
		  for new-level = (and chunk (level chunk))
		  when (= level new-level) do
		  (let ((index (inner-block rest))
			(inner-markup (html-inner-block-markup chunk)))
		    (render-block (subseq rest 0 index)
				  level inner-markup t)
		    (setf rest (nthcdr (1- index) rest)))
		  when (< level new-level) do
		  (multiple-value-bind (block remaining method)
		      (next-block rest new-level)
		    (declare (ignore method))
		    (render-block
		     block new-level (html-block-markup chunk) nil)
		    (setf rest remaining)))))
      (do-it (collect-elements (chunks document)) (level document)))))

(defmethod render-plain ((chunk chunk))
  (bind ((paragraph? (paragraph? chunk)))
    (iterate-elements
     (lines chunk)
     (lambda (line)
       (render-plain line)))
    (when paragraph?
      (fresh-line *output-stream*))))

(defmethod render-plain ((line string))
  (format *output-stream* "~a" line))

(defmethod render-plain ((chunk list))
  (render-span-plain (first chunk) (rest chunk)))

(defmethod render-span-plain ((code t) body)
  (format *output-stream* "~a" (first body)))

(defmethod render-span-plain ((code (eql 'eval)) body)
  (render-handle-eval body))

(defmethod render-span-plain ((code (eql 'code-eval)) body)
  (render-handle-eval body))
