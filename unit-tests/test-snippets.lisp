(in-package #:cl-markdown-test)

#|
Could use something like the form below to test the structure of the output
and thereby differentiate between parsing problems and output problems

(collect-elements 
 (chunks d) 
 :transform 
 (lambda (chunk)
   (list (started-by chunk) (ended-by chunk) 
	 (level chunk) (markup-class chunk))) 
 :filter (lambda (chunk) (not (ignore? chunk))))

|#


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
  header-paragraph-embedded-link
  (check-output 
   "## Common Lisp

* An item with a link [link](link.html) and some following text."))

;; test example from hhalvors@Princeton.EDU
(addtest (test-snippets)
  header-paragraph-embedded-link-in-list
  (check-output
   "## Common Lisp

* An item with a link [link](link.html) and some following text.
* Another item"))

;; test example from hhalvors@Princeton.EDU
(addtest (test-snippets)
  headers-and-lists
  (check-output
   "## Common Lisp

* An item with a link [link](link.html) and some following text.

## A second level heading

* Another item"))

(addtest (test-snippets)
  header-in-list
  ;; this is close 'enough' but not quite right.
  ;; I'm also including it because the results surprised me.
  (check-output
"* ok

    # eh"))

(addtest (test-snippets)
  list-item-with-hard-return
  (check-output
   "* A first list item
with a hard return
* A second list item
"))

(addtest (test-snippets)
  list-items-and-paragraphs-1
  (check-output
   "* first line

second line"))

(addtest (test-snippets)
  list-items-and-paragraphs-2
  (check-output
   "* first line

* second line"))

(addtest (test-snippets)
  list-items-and-paragraphs-3
  (check-output
   "* first line
* second line"))

(addtest (test-snippets)
  list-items-and-paragraphs-4
  (check-output
   "* first line
second line"))

(addtest (test-snippets)
  inline-html-1
  (check-output "`<div>foo</div>`"))

(addtest (test-snippets)
  inline-html-2
  (check-output "Simple block on one line:

    <div>foo</div>
"))

(deftestsuite test-lists-and-paragraphs (test-snippets)
  ())

(addtest (test-lists-and-paragraphs)
  list-item-with-paragraph
  (check-output "
* List item

    with another paragraph

        and some code

* Another item

this ends the list and starts a paragraph."))

;;;;;

(deftestsuite test-break (test-snippets)
  ()
  (:tests 
   (no-spaces(check-output "hello
there"))
   (one-space (check-output "hello 
there"))
   (two-spaces (check-output "hello  
there"))
   (three-spaces (check-output "hello   
there"))))

;;;;;

(deftestsuite entity-snippets (test-snippets)
  ())

(addtest (entity-snippets)
  entity-check-1
  (check-output "AT&T puts the amp in &amp; >boss<"))

(addtest (entity-snippets)
  entity-check-2
  (check-output "The AT&T is AT &amp; T, not AT&amp;T or AT &amp;T"))

(addtest (entity-snippets)
  entity-check-3
  (check-output "
Never forget AT
&amp;T"))


#|
(markdown "
Never forget AT
&amp;T")

(markdown 
"* List item

    with another paragraph

        and some code

* Another item

this ends the list and starts a paragraph.") 

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

#|

block level elements

; shell-markdown #+(or) 
(cl-markdown:markdown "<div class=\"header\">

Header text

</div>

# Heading

Some text
")


(shell-markdown "<div class=\"header\">

Header text

</div>
")

(shell-markdown "<h1>**hello**</h1>")
(shell-markdown "<div class=\"header\">

**Header text**

</div>
")

(shell-markdown "ok 
<div class=\"header\">

Header text

</div>
and now
")
|#

;;;;;

#|
(markdown "
For example, to add an HTML table to a Markdown article:

    This is a regular paragraph.

    <table>

        <tr>

            <td>
Foo</td>

        </tr>

    </table>


    This is another regular paragraph.

Note that Markdown formatting syntax is not processed within block-level
HTML tags. E.g., you can't use Markdown-style `*emphasis*` inside an
HTML block.")

(markdown "
For example, to add an HTML table to a Markdown article:

    This is a regular paragraph.

    <table>
        <tr>
            <td>Foo</td>
        </tr>
    </table>

    This is another regular paragraph.

Note that Markdown formatting syntax is not processed within block-level
HTML tags. E.g., you can't use Markdown-style `*emphasis*` inside an
HTML block.")

;; I think that the solution is too 
;; recode the line when you pop out a level.
(markdown "
For example, to add an HTML table to a Markdown article:

    This is a regular paragraph.

    <em>emphasis</em>

OK")

(shell-markdown "    AT&amp;T
OK")

|#