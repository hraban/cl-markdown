(in-package #:cl-markdown)

#|
Footnotes
{note foo}
{note "This is a note"}
{note "Foo"}
{note This is a note}

 [foo]> This is the note that goes with foo

That is what he thought.{footnote foo}

 [foo]> "This is a longer note with 
linefeeds, *mark-up*, and \\\"escaped\\\" quotes.
I'll be wicked surprised if it works out of the 
box."

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

|#


(defclass* footnote-text ()
  ((id nil ia)
   (text nil ia)))

(defun footnote (phase args result)
  ;; {documentation text}
  (declare (ignore result))
  (when (eq phase :parse)
    (let ((text (format nil "~{~a ~}" args)))
      (when text
	(format *output-stream* "~&<div class=\"documentation ~(~a~)\">" type)
	(markdown docs
		  :stream *output-stream*
		  :format *current-format*)
	(format *output-stream* "~&</div>"))

    
    (print args)))

(markdown 
 "That's right.{footnote Actually, it isn't, **but**
 that is how it goes.}"
 :additional-extensions '(footnote))

(markdown 
 "That's right.{footnote \"Actually, it isn't, but that is how it goes.\"}"
 :additional-extensions '(footnote))

    (bind (((thing &optional type) args)
	   (thing (let ((*package* (or (docs-package) *package*)))
		    (with-input-from-string (in thing) (read in))))
	   (type (or (and type 
			  (with-input-from-string (in type) (read in)))
		     (loop for type in '(function variable package setf
					      type structure compiler-macro
					      method-combination t)
			       when (documentation thing type) do
			       (return type))))
	   (docs (documentation thing type)))
      (when docs
	(format *output-stream* "~&<div class=\"documentation ~(~a~)\">" type)
	(markdown docs
		  :stream *output-stream*
		  :format *current-format*)
	(format *output-stream* "~&</div>"))
      nil)))


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
  footnote-text
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