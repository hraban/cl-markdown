(in-package cl-markdown)
(use-package 'lift)

#|
(run-tests :suite 'test-chunkers)
|#

(setf *lift-equality-test* 'samep)

(deftestsuite test-chunkers () ())

(deftestsuite test-line-is-empty-p (test-chunkers)
  ()
  (:test ((ensure (line-is-empty-p "      "))))
  (:test ((ensure (not (line-is-empty-p "       4")))))
  (:test ((ensure (not (line-is-empty-p "4  ")))))
  (:test ((ensure (line-is-empty-p (coerce (list #\tab #\space #\newline) 'string))))))

(deftestsuite test-line-s (test-chunkers)
  ()
  (:test ((ensure (line-starts-with-number-p "1."))))
  (:test ((ensure (not (line-starts-with-number-p "a.")))))
  (:test ((ensure (not (line-starts-with-number-p "1 hello")))))
  (:test ((ensure (line-starts-with-number-p "10123."))))
  (:test ((ensure (not (line-starts-with-number-p "10123th is big"))))))

;;; ---------------------------------------------------------------------------

(deftestsuite test-remove-bullet (test-chunkers) ()
  (:test ((ensure-same (remove-bullet "* hello") "hello")))
  (:test ((ensure-same (remove-bullet "*. hello") "hello")))
  (:test ((ensure-same (remove-bullet "*.      hello") "hello")))
  (:test ((ensure-same (remove-bullet "*     hello") "hello")))
  (:test ((ensure-same (remove-bullet "+.     hello") "hello")))
  (:test ((ensure-same (remove-bullet "-.     hello") "hello")))
  (:test ((ensure-same (remove-bullet "-.     ") ""))))

;;; ---------------------------------------------------------------------------

(deftestsuite test-remove-number (test-chunkers) ()
  (:test ((ensure-same (remove-number "1. hello") "hello")))
  (:test ((ensure-same (remove-number "232. hello") "hello")))
  (:test ((ensure-same (remove-number "3.      hello") "hello")))
  (:test ((ensure-same (remove-number "453.     hello") "hello")))
  (:test ((ensure-same (remove-number "2.") "")))
  (:test ((ensure-same (remove-number "123.     ") ""))))

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
  (:test ((ensure-same (line-indentation "hello  ") 0)))
  (:test ((ensure-same (line-indentation 
                        (coerce (list #\tab #\space #\h #\i) 'string)) 
                       (1+ *spaces-per-tab*)))))

;;; ---------------------------------------------------------------------------

(deftestsuite test-chunk-source (test-chunkers)
  ())

(addtest (test-chunk-source)
  simple-1
  (let ((document
         (chunk-source 
          "this is
paragraph number one.

this is paragraph number two.




and this
is
paragraph number
three.")))
    (ensure-same (size (chunks document)) 3)))

;;; ---------------------------------------------------------------------------

(addtest (test-chunk-source)
  simple-mixed-indenting-no-breaks
  (let ((document
         (chunk-source 
          "this is
  paragraph number one.
    this is paragraph number one
and this
    is
    paragraph number
        one")))
    (ensure-same (size (chunks document)) 1)))

;;; ---------------------------------------------------------------------------

(addtest (test-chunk-source)
  simple-bullets-no-breaks
  (let ((document
         (chunk-source 
          "this is a list
* item 1
* item 2
that's all.")))
    (ensure-same (size (chunks document)) 4)))

;;; ---------------------------------------------------------------------------

(addtest (test-chunk-source)
  simple-bullets-with-breaks
  (let ((document
         (chunk-source 
          "this is a list

* item 1
* item 2

that's all.")))
    (ensure-same (size (chunks document)) 4)))

;;; ---------------------------------------------------------------------------

(addtest (test-chunk-source)
  simple-multiline-bullets
  (let ((document
         (chunk-source 
          "this is a list

* item 1
is a bullet that take
many lines
* item 2

that's all.")))
    (ensure-same (size (chunks document)) 4)))

;;; ---------------------------------------------------------------------------

(addtest (test-chunk-source)
  simple-multiline-bullets-with-breaks
  (let ((document
         (chunk-source 
          "this is a list

* item 1

  is a bullet that take
many lines

  over three paragraphs

* item 2

that's all.")))
    (ensure-same (size (chunks document)) 6)
    (ensure-same (size (lines (nth-element (chunks document) 0))) 1)
    (ensure-same (size (lines (nth-element (chunks document) 2))) 2)
    (ensure-same (indentation (nth-element (chunks document) 3)) 2)
    (ensure-same (indentation (nth-element (chunks document) 4)) 0)))

;;; ---------------------------------------------------------------------------

(addtest (test-chunk-source)
  simple-bullets-and-numbers
  (let ((document
         (chunk-source 
          "this is a list

* of
* bullets
1. and numbers
2. and more numbers
+ and then bullets
- and more bullets

that's all.")))
    (ensure-same (size (chunks document)) 8)))

;;; ---------------------------------------------------------------------------

(addtest (test-chunk-source)
  simple-headers-1
  (let ((document
         (handle-setext-headers
          (chunk-source 
          "Random line
Title One
========

What not

========
Just some equal signs
"))))
    (ensure-same (size (chunks document)) 5)))

;;; ---------------------------------------------------------------------------

(addtest (test-chunk-source)
  simple-headers-1
  (let ((document
         (handle-setext-headers
          (chunk-source 
           "
Title
========
Subtitle
--------

What not is 
a good start to a paragraph.

"))))
    (ensure-same (size (chunks document)) 3)))

;;; ---------------------------------------------------------------------------
