(in-package #:cl-markdown-test)

#|
(run-tests :suite 'test-markdown)
|#

(deftestsuite cl-markdown-test () ())

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

;;;;;

(deftestsuite no-markdown-in-inline-html (cl-markdown-test)
  ()
  :equality-test #'string=)

(addtest (no-markdown-in-inline-html)
  no-emphasis
  (ensure-same
   (remove-if 'whitespacep
	      (nth-value
	       1 (markdown "Hi <img src='./images/lisplogo_flag_64.png' />"
			   :format :html :stream nil)))
   "<p>Hi<imgsrc='./images/lisplogo_flag_64.png'/></p>"))

;;;;;

(deftestsuite inline-html (cl-markdown-test)
  ()
  :equality-test #'string=)

(addtest (inline-html)
  do-not-encode
  (ensure-same
   (remove-if 'whitespacep
	      (nth-value
	       1 (markdown "Hi <em>there</em>"
			   :format :html :stream nil)))
   "<p>Hi<em>there</em></p>"))

(addtest (inline-html)
  encode-in-code
  (ensure-same
   (remove-if 'whitespacep
	      (nth-value
	       1 (markdown "Hi `<em>there</em>`"
			   :format :html :stream nil)))
   "<p>Hi<code>&lt;em&gt;there&lt;/em&gt;</code></p>"))

