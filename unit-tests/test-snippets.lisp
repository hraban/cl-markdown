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
  (bind (((:values result error status)
	  (shell-command 
	   (format nil "tidy --show-body-only 1 --quiet 1 ~
                 --show-warnings 0")
	   :input source)))
    (values result error status)))

(defun shell-markdown (source)
  (bind (((:values result error status)
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
     (bind (((:values doc text)
	     (markdown source :stream nil :format :html)))
       (setf *last-document* doc)
       ;; just get the first value
       (values (shell-tidy text)))
     (shell-tidy (shell-markdown source)) 
     :test (lambda (a b)
	     (compare-line-by-line a b :key 'cl-markdown::strip-whitespace 
				   :test 'string-equal))))))

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

(addtest (test-snippets)
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

(addtest (test-lists-and-paragraphs)
  nested-lists-with-hard-returns
  (check-output "
 * Item 1
   is spunky

    * Item A
"))

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

(deftestsuite test-bracket-processing (cl-markdown-test)
  ())

(addtest (test-bracket-processing)
  donot-process-code
  (let ((text 
	 (nth-value 1
		    (markdown "{set-property a \"set 1\"}

Paragraph 1

    Code 1
    {set-property a \"set 2\"}
    More code

All done: a = {property a}" :stream nil))))
    (ensure (search "a = set 1" text :test 'char=) 
	    :report "a set correctly")
    (ensure (search "set 2" text :test 'char=)
	    :report "code not mangled")))

(addtest (test-bracket-processing)
  double-brackets-for-code
  (let ((text 
	 (nth-value 1
		    (markdown "{set-property a \"set 1\"}

Paragraph 1

    Code 1
    {{set-property a \"set 2\"}}
    More code

All done: a = {property a}" :stream nil))))
    (ensure (search "a = set 2" text :test 'char=) 
	    :report "a set correctly")
    (ensure-null (search "a \"set 2" text :test 'char=)
		 :report "code not mangled")))  

(deftestsuite brackets-with-empty-lines (test-bracket-processing)
  ())

(addtest (brackets-with-empty-lines)
  no-linefeeds
  (ensure (search "footnoteBacklink"
		  (nth-value 1 (markdown "
Hi there
this is a footnote{footnote \"Actualy, this is\"}. Nice.

{footnotes}" :stream nil)) :test #'char=)))

(addtest (brackets-with-empty-lines)
  one-linefeed
  (ensure (search "footnoteBacklink"
		  (nth-value 1 (markdown "
Hi there
this is a footnote{footnote \"Actualy, 
this is\"}. Nice.

{footnotes}" :stream nil)) :test #'char=)))


(addtest (brackets-with-empty-lines)
  two-linefeeds
  (ensure (search "footnoteBacklink"
		  (nth-value 1 (markdown "
Hi there
this is a footnote{footnote \"Actualy, 

this is\"}. Nice.

{footnotes}" :stream nil)) :test #'char=)))

(addtest (brackets-with-empty-lines)
  linefeed-in-bracket
  (ensure (search "guide for test 3.0"
		  (nth-value 1 
			     (markdown "{set-property test \"3.0\"}

This is the guide for test {property
test}. It rocks." :stream nil))
		  :test 'char=)))

;;;;

(deftestsuite brackets-and-includes (cl-markdown-test)
  ((temporary-directory "/tmp/"))
  (:setup 
   (with-new-file (out (relative-pathname temporary-directory "bandi-1.md"))
     (format out "
{set-property slush \"1234-simple\"}
This is true.\{footnote \"technically, this is true\"}. Did you:

 * like it?
 * love it?
 * find it irrelevant?

"))
   (with-new-file (out (relative-pathname temporary-directory "bandi-2.md"))
     (format out "
{set-property slush \"1234-complex\"}
This is true.\{footnote \"actually it's not
only false but also

 1. misleading
 2. incorrect
 3. overly optimistic.

Let you conscience by your guide.\"}"))))

(addtest (brackets-and-includes)
  include-simple
  (let ((output 
	 (nth-value 1
		    (markdown 
		     (concatenate 'string
				  "Including bandi-1.md

{include " (namestring (relative-pathname temporary-directory "bandi-1.md")) "}

slush: {property slush}

Lets show the footnotes:

{footnotes}

All done.")
		     :stream nil))))
    (ensure (search "like it?" output :test 'char=)
	    :report "footnote not found")
    (ensure (search "1234-simple" output :test 'char=)
	    :report "property not found")))


(addtest (brackets-and-includes)
  include-complex
  (let ((output 
	 (nth-value 1
		    (markdown 
		     (concatenate 'string
				  "Including bandi-2.md

{include " (namestring (relative-pathname temporary-directory "bandi-2.md")) "}

slush: {property slush}

Lets show the footnotes:

{footnotes}

All done.")
		     :stream nil))))
    (ensure (search "misleading" output :test 'char=)
		    :report "footnote not found")
    (ensure (search "1234-complex" output :test 'char=)
	    :report "property not found")))

;;;;

(deftestsuite include-if (cl-markdown-test)
  ((temporary-directory "/tmp/"))
  (:setup
   (with-new-file (out (relative-pathname temporary-directory "bandi-1.md"))
     (format out "
{set-property slush \"1234-simple\"}
This is true.\{footnote \"technically, this is true\"}. Did you:

 * like it?
 * love it?
 * find it irrelevant?

"))))

(addtest (include-if)
  property-not-set
  (let ((text
	 (nth-value
	  1 (markdown 
	     (concatenate 
	      'string "# Title

{include-if test-prop " 
	      (namestring (relative-pathname temporary-directory "bandi-1.md"))
	      "} 

paragraph") :stream nil :properties `((test-prop . nil))))))
    (ensure-null (search "This is true" text :test 'char=))))

(addtest (include-if)
  property-set
  (let ((text
	 (nth-value
	  1 (markdown 
	     (concatenate 
	      'string "# Title

{include-if test-prop " 
	      (namestring (relative-pathname temporary-directory "bandi-1.md"))
	      "} 

paragraph") :stream nil :properties `((test-prop . t))))))
    (ensure (search "This is true" text :test 'char=))))


;;;;

(deftestsuite nested-properties (cl-markdown-test)
  ((temporary-directory "/tmp/"))
  (:setup
   (with-new-file (out (relative-pathname temporary-directory "bandi-1.md"))
     (format out "
{set-property slush \"1234-simple\"}
"))))

(addtest (nested-properties)
  try-it
  (let ((text
	 (nth-value
	  1 (markdown 
"a {set-property a \"alpha\"}
 b {set-property b \"{property a}\"}
 c {set-property c \"a is {property b}\"}

{property b}

# Title is {property c}

Hi there" :stream nil))))
    (ensure (search "Title is a is alpha" text :test 'char=))))

(addtest (nested-properties)
  works-with-included-documents-too
  (let ((text
	 (nth-value
	  1 (markdown 
	     (concatenate 
	      'string "
{include " (namestring (relative-pathname temporary-directory "bandi-1.md"))
 "}
a {set-property a \"this is {property slush} too\"}
 b {set-property b \"{property a}\"}
 c {set-property c \"a is {property a}\"}

# Title is {property c}

Hi there") :stream nil))))
    (ensure (search "Title is a is this is 1234-simple" text :test 'char=))))

