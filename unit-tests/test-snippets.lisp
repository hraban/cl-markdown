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

(addtest (test-lists-and-paragraphs)
  mingling-text-and-code
  (check-output "
para

    code

para

    code

"))


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

(addtest (test-lists-and-paragraphs
	  :expected-failure "Markdown views treats the 1. as a *.")
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

(addtest (test-lists-and-paragraphs)
  nested-lists-3
  (check-output "
* a
    * b
    * c 
* d
"))

(addtest (test-lists-and-paragraphs)
  nested-lists-4
  (check-output "
* a
* b
    * c 
    * d
"))

(addtest (test-lists-and-paragraphs)
  nested-lists-with-hard-returns
  (check-output "
 * Item 1
   is spunky

    * Item A
"))

(addtest (test-lists-and-paragraphs)
  lists-and-code-1
  (ensure-same
   (nth-value 1 
	      (cl-markdown:markdown
	       "
* The select form rewrites... If we add another line.

        (select (?x) 
          (q ?x !property node))

"))
   (nth-value 1 
	      (cl-markdown:markdown
	       "
* The select form rewrites...
If we add another line.

        (select (?x) 
          (q ?x !property node))

")) :test 'string=))

(addtest (test-lists-and-paragraphs
	  :expected-failure "paragraphs")
  lists-and-blockquote
  (check-output       "paragraph 1

> ok 

* item 1

    q2. I thiought I had this one

ok"))

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

(addtest (numbered-lists)
  nospace
  (check-output "
  1.hi
  2.there
"))

(addtest (numbered-lists 
	  :expected-failure "Looks like a markdown bug")
  nocontents
  (check-output "
  1. 
  2. 
"))
;;;;

(deftestsuite test-horizontal-rules (test-snippets)
  ())

(addtest (test-horizontal-rules)
  horizontal-rules-1
  (check-output
  "Here are some rules.
I hope you like 'em.

---
***
- - -
** ** **
_ _ _____ _ _

Did you like them?"))

(addtest (test-horizontal-rules)
  horizontal-rules-2
  (ensure (search 
   "this is code" 
   (nth-value 1 (markdown:markdown  
		 "Here is an example:

    this is code

 - - - -
" :stream nil)) :test 'char=)))

;;;;

(deftestsuite test-nested-lists (test-snippets)
  ())

(addtest (test-nested-lists)
  three-deep
  (check-output 
   "
* a
    * b
        * c"))


(deftestsuite test-blockquotes (test-snippets)
  ())

(addtest (test-blockquotes)
  nested-1
  (check-output 
   "
> a
> b
"))

(addtest (test-blockquotes)
  nested-2
  (check-output 
   "
> a

> b
"))

(addtest (test-blockquotes)
  nested-3
  (check-output 
   "
> a

>> b
"))


;;;;;


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

(addtest (test-snippets)
  reference-link-text-with-line-breaks
  (check-output 
   "Hi this [is
so][is-so] cool.

 [is-so]: http://a.c.c/"))

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

(addtest (test-snippets)
  inline-html-2
  (check-output "Simple block on one line:

    <div>foo</div>
"))

