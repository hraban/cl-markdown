(in-package #:cl-markdown-test)

#|
(run-tests :suite 'test-chunkers :break-on-errors? t)

|#

(deftestsuite test-it-starts-with-block-level-html-p (cl-markdown-test)
  ()
  (:tests 
   ((ensure (not (it-starts-with-block-level-html-p "<uck>"))))
   ((ensure (not (it-starts-with-block-level-html-p " <div>"))))
   ((ensure (not (it-starts-with-block-level-html-p "hello"))))
   ((ensure (not (it-starts-with-block-level-html-p "<divvy>"))))
   ((ensure (not (it-starts-with-block-level-html-p "<div "))))
   ((ensure (it-starts-with-block-level-html-p "<div>")))
   ((ensure (it-starts-with-block-level-html-p "<div class=\"foo\">")))
   ((ensure (it-starts-with-block-level-html-p "<ul>")))
   ((ensure (not (it-starts-with-block-level-html-p "<>"))))

   ((ensure (not (it-starts-with-block-level-html-p "</uck>"))))
   ((ensure (not (it-starts-with-block-level-html-p " </div>"))))
   ((ensure (not (it-starts-with-block-level-html-p "/hello"))))
   ((ensure (not (it-starts-with-block-level-html-p "</divvy>"))))
   ((ensure (not (it-starts-with-block-level-html-p "</div "))))
   ((ensure (it-starts-with-block-level-html-p "</div>")))
   ((ensure (it-starts-with-block-level-html-p "</ul>")))))
  
  

;;;;

(deftestsuite test-chunkers (cl-markdown-test)
  ()
  (:equality-test 'samep))

(deftestsuite test-line-is-empty-p (test-chunkers)
  ()
  (:test ((ensure (line-is-empty-p "      "))))
  (:test ((ensure (not (line-is-empty-p "       4")))))
  (:test ((ensure (not (line-is-empty-p "4  ")))))
  (:test ((ensure (line-is-empty-p 
		   (coerce (list #\tab #\space #\newline) 'string))))))

(deftestsuite line-starts-with-number-p (test-chunkers)
  ()
  (:test ((ensure-null (line-starts-with-number-p "1."))))
  (:test ((ensure (not (line-starts-with-number-p "a.")))))
  (:test ((ensure (not (line-starts-with-number-p "1 hello")))))
  (:test ((ensure-null (line-starts-with-number-p "10123."))))
  (:test ((ensure (not (line-starts-with-number-p "10123th is big"))))))

;;; ---------------------------------------------------------------------------

(deftestsuite line-starts-with-bullet-p (test-chunkers)
  ()
  (:test ((ensure (line-starts-with-bullet-p "* hello"))))
  (:test ((ensure (not (line-starts-with-bullet-p "  *.")))))
  (:test ((ensure (not (line-starts-with-bullet-p "*")))))
  (:test ((ensure (line-starts-with-bullet-p "   * "))))
  (:test ((ensure (not (line-starts-with-bullet-p "   *")))))
  (:test ((ensure (not (line-starts-with-bullet-p "    *"))))))

;;; ---------------------------------------------------------------------------

(deftestsuite test-remove-marker (test-chunkers) ()
  (:test ((ensure-same (remove-marker "* hello") "hello")))
  (:test ((ensure-same (remove-marker "*. hello") "hello")))
  (:test ((ensure-same (remove-marker "*.      hello") "hello")))
  (:test ((ensure-same (remove-marker "*     hello") "hello")))
  (:test ((ensure-same (remove-marker "+.     hello") "hello")))
  (:test ((ensure-same (remove-marker "-.     hello") "hello")))
;  (:test ((ensure-same (remove-marker "  -.     hello") "hello")))
  (:test ((ensure-same (remove-marker "-.     ") ""))))

;;; ---------------------------------------------------------------------------

(deftestsuite test-remove-number (test-chunkers) ()
  (:test ((ensure-same (remove-number "1. hello") "hello")))
  (:test ((ensure-same (remove-number "232. hello") "hello")))
  (:test ((ensure-same (remove-number "3.      hello") "hello")))
  (:test ((ensure-same (remove-number "453.     hello") "hello")))
  (:test ((ensure-same (remove-number "2.") "")))
  (:test ((ensure-same (remove-number "123.     ") ""))))

;;; ---------------------------------------------------------------------------

(deftestsuite test-line-is-horizontal-rule-p (test-chunkers) ()
  (:test ((ensure (line-is-horizontal-rule-p "---"))))
  (:test ((ensure (line-is-horizontal-rule-p "- - -"))))
  (:test ((ensure (line-is-horizontal-rule-p "  -  -  -  "))))
  (:test ((ensure (line-is-horizontal-rule-p "  -     --"))))
  (:test ((ensure-null (line-is-horizontal-rule-p " =     =     ="))))
  (:test ((ensure (line-is-horizontal-rule-p "__      _"))))
  (:test ((ensure (not (line-is-horizontal-rule-p "-_-")))))
  )

;;; ---------------------------------------------------------------------------

(deftestsuite test-atx-header-markup-class (test-chunkers) ()
  (:test ((ensure-same (atx-header-markup-class "# hello #") 'header1)))
  (:test ((ensure-same (atx-header-markup-class "###### hello #") 'header6)))
  (:test ((ensure-error (atx-header-markup-class "####### hello #"))))
  (:test ((ensure-error (atx-header-markup-class "h###ello")))))

;;; ---------------------------------------------------------------------------

(deftestsuite test-remove-atx-header (test-chunkers) ()
  (:test ((ensure-same (remove-atx-header "# hello #") "hello")))
  (:test ((ensure-same (remove-atx-header "###### hello #") "hello")))
  (:test ((ensure-same (remove-atx-header "### ### hello") "### hello"))))
  
;;; ---------------------------------------------------------------------------

(deftestsuite test-line-indentation (test-chunkers)
  ()
  (:test ((ensure-same (line-indentation "  hello") 2)))
  (:test ((ensure-same (line-indentation "") 0)))
  (:test ((ensure-same (line-indentation "hello  ") 0)))
  (:test ((ensure-same (line-indentation 
                        (coerce (list #\tab #\space #\h #\i) 'string)) 
                       (1+ *spaces-per-tab*)))))

;;; ---------------------------------------------------------------------------

(deftestsuite test-line-is-code-p (test-chunkers)
  ()
  (:test ((ensure (line-is-code-p "    hello"))))
  (:test ((ensure (line-is-code-p (format nil "~Chello" #\Tab)))))
  (:test ((ensure (not (line-is-code-p "hello"))))))

;;; ---------------------------------------------------------------------------

(deftestsuite test-line-is-blockquote-p (test-chunkers)
  ()
  (:test ((ensure (line-is-blockquote-p "> hello"))))
  (:test ((ensure (line-is-blockquote-p " > hello"))))
  (:test ((ensure (line-is-blockquote-p "  > hello"))))
  (:test ((ensure (line-is-blockquote-p "   > hello"))))
  (:test ((ensure (not (line-is-blockquote-p "    > hello"))))))

;;; ---------------------------------------------------------------------------

(deftestsuite test-one-tab-stripper (test-chunkers) ()
  (:test ((ensure-same (one-tab-stripper "hello") 
                       (values "hello" nil))))
  (:test ((ensure-same (one-tab-stripper "    hello")
                       (values "hello" t))))
  (:test ((ensure-same (one-tab-stripper "        hello")
                       (values "    hello" t)))))

;;; ---------------------------------------------------------------------------

(deftestsuite test-blockquote-stripper (test-chunkers) ()
  (:test ((ensure-same (blockquote-stripper "hello") 
                       (values "hello" nil))))
  (:test ((ensure-same (blockquote-stripper "> hello") 
                       (values "hello" t))))
  (:test ((ensure-same (blockquote-stripper ">") 
                       (values "" t))))
  (:test ((ensure-same (blockquote-stripper " > > why") 
                       (values "> why" t))))
  (:test ((ensure-same (blockquote-stripper "  >  > why") 
                       (values " > why" t))))
  (:test ((ensure-same (blockquote-stripper "    >> why") 
                       (values "    >> why" nil)))))

(deftestsuite test-maybe-strip-line (test-chunkers)
  ()
  (:setup (reset *parsing-environment*)))

(addtest (test-maybe-strip-line)
  no-strippers
  (ensure-same (maybe-strip-line "hello") (values "hello" 0))
  (ensure-same (maybe-strip-line "    hello") (values "    hello" 0)))

;;; ---------------------------------------------------------------------------

(deftestsuite test-maybe-strip-line-one-tab-stripper (test-maybe-strip-line)
  ()
  (:setup (insert-item (strippers *parsing-environment*) 'one-tab-stripper))
  (:test ((ensure-same (maybe-strip-line "hello") (values "hello" 0))))
  (:test ((ensure-same (maybe-strip-line "    hello") (values "hello" 1))))
  (:test ((ensure-same (maybe-strip-line "    * hello") (values "* hello" 1))))
  (:test ((ensure-same (maybe-strip-line "        hello") (values "    hello" 1)))))

;;; ---------------------------------------------------------------------------

(deftestsuite test-maybe-strip-line-two-tab-strippers (test-maybe-strip-line)
  ()
  (:setup 
   (insert-item (strippers *parsing-environment*) 'one-tab-stripper)
   (insert-item (strippers *parsing-environment*) 'one-tab-stripper))
  (:test ((ensure-same (maybe-strip-line "hello") (values "hello" 0))))
  (:test ((ensure-same (maybe-strip-line "    hello") (values "hello" 1))))
  (:test ((ensure-same (maybe-strip-line "    * hello") (values "* hello" 1))))
  (:test ((ensure-same (maybe-strip-line "        hello") (values "hello" 2))))
  (:test ((ensure-same (maybe-strip-line "            hello")
		       (values "    hello" 2)))))

(deftestsuite test-maybe-strip-line-one-bq-strippers (test-maybe-strip-line)
  ()
  (:setup 
   (insert-item (strippers *parsing-environment*) 'blockquote-stripper))
  (:test ((ensure-same (maybe-strip-line "hello") (values "hello" 0))))
  (:test ((ensure-same (maybe-strip-line "> hello") (values "hello" 1))))
  (:test ((ensure-same (maybe-strip-line ">> hello") (values "> hello" 1)))))

(deftestsuite test-maybe-strip-line-two-bq-strippers 
  (test-maybe-strip-line-one-bq-strippers)
  ()
  (:setup 
   (insert-item (strippers *parsing-environment*) 'blockquote-stripper))
  (:test ((ensure-same (maybe-strip-line "hello") (values "hello" 0))))
  (:test ((ensure-same (maybe-strip-line "> hello") (values "hello" 1))))
  (:test ((ensure-same (maybe-strip-line ">> hello") (values "hello" 2)))))

;;?? FiXME -- why?!
#+(or)
(deftestsuite test-chunk-source (test-chunkers)
  ((document (progn (princ "******") (make-container 'cl-markdown::document)))))

(deftestsuite test-chunk-source (test-chunkers)
  (document)
  (:setup
   (setf document (make-container 'cl-markdown::document))))

(addtest (test-chunk-source)
  simple-1
  (chunk-source 
   document
   "this is
paragraph number one.

this is paragraph number two.




and this
is
paragraph number
three.")
    (ensure-same (size (chunks document)) 3))

(addtest (test-chunk-source)
  simple-mixed-indenting-no-breaks
  (chunk-source 
   document "this is
  paragraph number one.
   this is paragraph number one
and this
   is
   paragraph number
 one")
    (ensure-same (size (chunks document)) 1))

(addtest (test-chunk-source)
  simple-bullets-with-breaks
  (chunk-source 
   document "this is a list

* item 1
* item 2

that's all.")
  (ensure-same (size (chunks document)) 4))

;;; ---------------------------------------------------------------------------

(addtest (test-chunk-source)
  simple-multiline-bullets
         (chunk-source 
   document "this is a list

* item 1
is a bullet that take
many lines
* item 2

that's all.")
  (ensure-same (size (chunks document)) 4))

;;; ---------------------------------------------------------------------------

(addtest (test-chunk-source)
  simple-multiline-bullets-with-breaks
         (chunk-source 
   document "this is a list

* item 1

  is a bullet that take
many lines

  over three paragraphs

* item 2

that's all.")
    (ensure-same (size (chunks document)) 6)
    (ensure-same (size (lines (nth-element (chunks document) 0))) 1)
    (ensure-same (size (lines (nth-element (chunks document) 2))) 2)
    (ensure-same (indentation (nth-element (chunks document) 3)) 2)
  (ensure-same (indentation (nth-element (chunks document) 4)) 0))

;;; ---------------------------------------------------------------------------

(addtest (test-chunk-source)
  simple-bullets-and-numbers
         (chunk-source 
   document "this is a list

* of
* bullets
1. and numbers
2. and more numbers
+ and then bullets
- and more bullets

that's all.")
  (ensure-same (size (chunks document)) 8))

;;; ---------------------------------------------------------------------------

(addtest (test-chunk-source)
  simple-headers-1
         (handle-setext-headers
          (chunk-source 
    document "Random line
Title One
========

What not

========
Just some equal signs
"))
  (ensure-same (size (chunks document)) 4))

(addtest (test-chunk-source)
  simple-headers-2
         (handle-setext-headers
          (chunk-source 
    document "
Title
========
Subtitle
--------

What not is 
a good start to a paragraph.

"))
  (ensure-same (size (chunks document)) 3))

;;; ---------------------------------------------------------------------------
;;; line-could-be-link-reference-title-p
;;; ---------------------------------------------------------------------------

(deftestsuite line-could-be-link-reference-title-p (test-chunkers)
  ()
  (:test ((ensure (line-could-be-link-reference-title-p "   \"Hi\""))))
  (:test ((ensure (line-could-be-link-reference-title-p "\"Hi\""))))
  (:test ((ensure (not (line-could-be-link-reference-title-p "   He said \"hi\"")))))
  (:test ((ensure (not (line-could-be-link-reference-title-p "   \"no closing quote"))))))