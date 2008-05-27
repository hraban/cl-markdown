(in-package #:cl-markdown-test)

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