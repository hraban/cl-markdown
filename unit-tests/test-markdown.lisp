(in-package #:cl-markdown-test)

#|
(run-tests :suite 'test-markdown)
|#

(deftestsuite cl-markdown-test-all () ())

(deftestsuite cl-markdown-test (cl-markdown-test-all) ())

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

