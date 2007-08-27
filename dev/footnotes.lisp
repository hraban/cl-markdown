(in-package #:cl-markdown)

#|
To do:

- allow footnotes to appear on a completely separate page
- do footnotes as a popup window with mouse over
- handle footnotes 'out of band' a la links

Footnotes
{note foo}
{note "This is a note"}
{note "Foo"}
{note This is a note}

(markdown
 "That is what he thought.{footnote foo}

 [foo]> \"This is a longer note with 
linefeeds, *mark-up*, and \\\"escaped\\\" quotes.
I'll be wicked surprised if it works out of the 
box.\"
")

Need to 

1. get a number
2. add link where the footnote starts
3. add anchor where the footnote starts
4. add footnote text at bottom of document / separate page
5. add link back to anchor in footnote

Our footnote HTML is so heavily influenced by DF that you might think
we just copied it all.

(markdown "
Maybe people{footnote Well, at least one person} find CL-Markdown 
to be the bees knees, the cats pajamas and the gnats goulash. In 
fact, if computers could dance, you could tell that one had 
CL-Markdown installed on it just by watching.{footnote Not really.}

{footnotes}

This was generated {today} at {now}.")
|#

(defclass* footnote-info ()
  ((id nil ia)
   (text nil ia)
   (reference-name nil ia)
   (name nil ia)))

;; provides an example of using result during render phase
(defun footnote (phase args result)
  ;; {documentation text}
  (let ((footnotes 
	 (or (document-property :footnote)
	     (setf (document-property :footnote)
		   (make-instance 'vector-container)))))
    (cond ((eq phase :parse)
	   (let* ((text (format nil "~{~a ~}" args)))
	     (when text
	       (bind ((id (size footnotes))
		      (fn-basename 
		       (format nil "~d-~a"
			       id
			       (format-date "%Y-%m-%d" 
					    (document-property 
					     :date-modified
					     (get-universal-time)))))
		      (fn-name (format nil "fn~a" fn-basename))
		      (ref-name (format nil "fnr~a" fn-basename)))
		 (insert-item footnotes
			      (make-instance
			       'footnote-info
			       :id id
			       :name fn-name
			       :reference-name ref-name
			       :text text))
		 (values id)))))
	((eq phase :render)
	 (let ((footnote (item-at footnotes (first result))))
	   (format *output-stream*
		   "<sup><a href=\"#~a\">~d</a></sup>"
		   (name footnote)
		   (1+ (id footnote))))))))
    
(defun footnotes (phase args result)
  (declare (ignore args result))
  (ecase phase
    (:parse)
    (:render
     (unless (empty-p (document-property :footnote))
       (format *output-stream* "~&<div class=\"footnotes\">")
       (format *output-stream* "~&<ol>")
       (iterate-elements
	(document-property :footnote)
	(lambda (footnote)
	  (output-anchor (name footnote))
	  (format *output-stream* "~&<li>")
	  (markdown (text footnote)
		    :stream *output-stream*
		    :format *current-format*
		    :properties '((:html . nil) 
				  (:omit-final-paragraph . t)
				  (:omit-initial-paragraph . t)))
	  (format *output-stream* "<a href=\"#~a\" class=\"footnoteBacklink\""
		  (reference-name footnote))
	  (format *output-stream* 
		  " title=\"Jump back to footnote ~d in the text\""
		  (1+ (id footnote)))
	  (format *output-stream* ">&#8617;</a></li>")))
       (format *output-stream*
	       "~&</ol>~&</div>")))))

;; not yet
#|
(defun handle-footnote-links (document)
  (iterate-elements
   (chunks document)
   (lambda (chunk)
     (when (line-is-footnote-text-p)
       (bind (((values nil link-info) 
               (scan-to-strings '(:sequence footnote-text)
				(first-element (lines chunk))))
              (id (aref link-info 0))
              (text (aref link-info 1)))
         (setf (item-at (link-info document) id)
               (make-instance 'footnote-text
                 :id id :title text)
               (ignore? chunk) t)))))
  ;; now remove the unneeded chunks
  (removed-ignored-chunks? document)
  document)

(defun line-is-footnote-text-p (line)
  (scan #.(ppcre:create-scanner '(:sequence footnote-text)) line))

(define-parse-tree-synonym
  footnote-label
    (:sequence
     :start-anchor
     (:greedy-repetition 0 3 :whitespace-char-class)
     bracketed
     #\> 
     (:greedy-repetition 0 nil :whitespace-char-class)
     (:register
      (:alternation
       (:sequence
	#\" (:greedy-repetition 0 nil (:inverted-char-class #\") #\"))
       (:greedy-repetition 0 nil :everything)))))

#+(or)
(scan-to-strings
 (create-scanner 'footnote-label)
 " [a]> why are you here
ok")

#+(or)
(scan-to-strings
 (create-scanner 'footnote-label)
 " [a]> \"why are you here?
I am here because that is why.

OK? ok!\"")
		      
|#