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

(defvar *last-document* nil)

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
    (ensure-same 
     (bind (((values doc text)
	     (markdown source :stream nil :format :html)))
       (setf *last-document* doc)
       (shell-tidy text))
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
  (check-output
"* ok

    # eh"))

#+(or)
(markdown 
 "* ok

    # eh")

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

(addtest (test-snippets :expected-failure "Problem is only in the whitespace")
  inline-html-2
  (check-output "Simple block on one line:

    <div>foo</div>
"))

(deftestsuite test-escapes (test-snippets)
  ())

(addtest (test-escapes)
  catch-markdown-ones
  (check-output "\\\\ \\` \\* \\_ \\[ \\] \\( \\) \\# \\. \\! \\>"))

(addtest (test-escapes)
  catch-markdown-ones-2
  (ensure-cases (var)
      '(("\\") ("`") ("*") ("_") 
	("[") ("]") ("(") (")")
	("#") (".") ("!")
	(">"))
    (check-output (format nil "hi \\~a dude" var))))  

(addtest (test-escapes :expected-failure "Problem in test suite... Markdown output is bad")
  catch-markdown-ones-<
  (check-output "\\<"))

(addtest (test-escapes)
  code-and-escapes
  (check-output "`\\*hi\\*`"))

(addtest (test-escapes)
  star-and-escapes
  (check-output "*\\*hi\\**"))


(deftestsuite test-lists-and-paragraphs (test-snippets)
  ())

(addtest (test-lists-and-paragraphs)
  list-item-with-paragraph-1
  (check-output "
* List item

    with another paragraph

        and some code

* Another item

this ends the list and starts a paragraph."))

#+(or)
(markdown
"
* List item

    with another paragraph

        and some code

* Another item

this ends the list and starts a paragraph.")

(addtest (test-lists-and-paragraphs)
  list-item-with-paragraph-2
  (check-output
   "
* List item

        and some code
"))

(addtest (test-lists-and-paragraphs)
  list-item-with-paragraph-3
  (check-output "
* Another item

    paragraph "))

(addtest (test-lists-and-paragraphs)
  list-item-with-paragraph-4
  (check-output "
* Item 1

    paragraph 1

* Item 2

    paragraph 2 

The end"))

(addtest (test-lists-and-paragraphs)
  list-item-with-paragraph-5
  (check-output "
* Item 1

1. paragraph 1"))

(addtest (test-lists-and-paragraphs)
  nested-lists-1
  (check-output "
* Item 1

    * Item A"))

;;?? Paragraph logic reversed?
(addtest (test-lists-and-paragraphs)
  nested-lists-2
  (check-output "
* Item 1

    * Item A

* Item 2"))

;;;;;

(deftestsuite test-break (test-snippets)
  ()
  (:tests 
   (no-spaces (check-output "hello
there"))
   (one-space (check-output "hello 
there"))
   (two-spaces (check-output "hello  
there"))
   (three-spaces (check-output "hello   
there"))))

;; NOTE: markdown doesn't add the <br /> unless there is content _after_
;; line that ends with two spaces...
(addtest (test-break)
  rest-of-line
  (check-output "this is **strong**  
ok?"))

(addtest (test-break)
  rest-of-line-2
  (check-output "this _is_ **strong**  
ok?"))

(addtest (test-break :expected-failure "markdown doesn't add the <br /> unless there is content _after_ line that ends with two spaces...")
  rest-of-line-3
  (check-output "this _is_ **strong**  "))

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

;;;;

(deftestsuite numbered-lists (test-snippets)
  ())

(addtest (numbered-lists)
  at-margin
  (check-output "
1. hi
2. there
"))

(addtest (numbered-lists)
  indented
  (check-output "
  1. hi
  2. there
"))




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