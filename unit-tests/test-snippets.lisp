(in-package #:cl-markdown-test)

(defun shell-tidy (source)
  (bind (((values result error status)
	  (shell-command 
	   (format nil "tidy --show-body-only 1 --quiet 1 ~
                 --show-warnings 0")
	   :input source)))
    (values result error status)))

(defun shell-markdown (source)
  (bind (((values result error status)
	  (shell-command 
	   (format nil "markdown")
	   :input source)))
    (values result error status)))

(deftestsuite test-snippets (cl-markdown-test)
  ()
  :equality-test #'string-equal
  (:function 
   (check-html-output
    (source html)
    (ensure-same 
     (shell-tidy
      (nth-value 
       1 (markdown source :stream nil :format :html)))
     (shell-tidy html) :test 'samep)))
  (:function 
   (check-output 
    (source)
    (ensure-same (shell-tidy
		  (nth-value 
		   1 (markdown source :stream nil :format :html)))
		 (shell-tidy (shell-markdown source)) 
		 :test 'samep))))

;; test example from hhalvors@Princeton.EDU
(addtest (test-snippets)
  (check-output 
   "## Common Lisp

* An item with a link [link](link.html) and some following text."))

;; test example from hhalvors@Princeton.EDU
(addtest (test-snippets)
  (check-output
   "## Common Lisp

* An item with a link [link](link.html) and some following text.
* Another item"))

;; test example from hhalvors@Princeton.EDU
(addtest (test-snippets)
  (check-output
   "## Common Lisp

* An item with a link [link](link.html) and some following text.

## A second level heading

* Another item"))

(addtest (test-snippets)
  (check-output
   "* A first list item
with a hard return
* A second list item
"))

(addtest (test-snippets)
  (check-output
   "* first line

second line"))

(addtest (test-snippets)
  (check-output
   "* first line

* second line"))

(addtest (test-snippets)
  (check-output
   "* first line
* second line"))

(addtest (test-snippets)
  (check-output
   "* first line
second line"))

(addtest (test-snippets)
  (check-output "AT&T puts the amp in &amp; >boss<"))

(addtest (test-snippets)
  inline-html
  (check-output "`<div>foo</div>`"))

(addtest (test-snippets)
  inline-html
  (check-output "Simple block on one line:

    <div>foo</div>
"))

#|
(markdown "Simple block on one line:

<div>foo</div>" :format :html :stream t)

"Here's another:

1. First
2. Second:
	* Fee
	* Fie
	* Foe
3. Third"

"1.	Item 1, graf one.

	Item 2. graf two. The quick brown fox jumped over the lazy dog's
	back.
	
2.	Item 2.

3.	Item 3."

"Same thing but with paragraphs:

1. First

2. Second:
	* Fee
	* Fie
	* Foe

3. Third"

; file:///Users/gwking/darcs/cl-markdown/website/output/comparison-tests/Backslash%20escapes-compare.html

|#