(in-package cl-markdown)
(use-package 'lift)

#|
(run-tests :suite 'test-markdown)
|#

(deftestsuite test-markdown () ())

;;; ---------------------------------------------------------------------------

(addtest (test-markdown)
  simple-1
  (let ((document
         (markdown 
          "this is
paragraph number one.

this is paragraph number two.




and this
is
paragraph number
three.")))
    (ensure-same (size (chunks document)) 3)
    (ensure-same (markup-classes (nth-element (chunks document) 1)) '(paragraph))
    (ensure-same (markup-classes (nth-element (chunks document) 2)) '(paragraph))))

;;; ---------------------------------------------------------------------------

(addtest (test-markdown)
  simple-mixed-indenting-no-breaks
  (let ((document
         (markdown 
          "this is
  paragraph number one.
    this is paragraph number one
and this
    is
    paragraph number
        one")))
    (ensure-same (size (chunks document)) 1)
    (ensure-same (markup-classes (nth-element (chunks document) 0)) '(paragraph))))

;;; ---------------------------------------------------------------------------

(addtest (test-markdown)
  simple-bullets-no-breaks
  (let ((document
         (markdown 
          "this is a list
* item 1
* item 2
that's all.")))
    (ensure-same (size (chunks document)) 4)
    (ensure-same (markup-classes (nth-element (chunks document) 0)) '(paragraph))
    (ensure-same (markup-classes (nth-element (chunks document) 1)) '(bullet))
    (ensure-same (markup-classes (nth-element (chunks document) 3)) '(paragraph))))

;;; ---------------------------------------------------------------------------

(addtest (test-markdown)
  simple-bullets-with-breaks
  (let ((document
         (markdown 
          "this is a list

* item 1
* item 2

that's all.")))
    (ensure-same (size (chunks document)) 4)
    (ensure-same (markup-classes (nth-element (chunks document) 0)) '(paragraph))
    (ensure-same (markup-classes (nth-element (chunks document) 1)) '(bullet))
    (ensure-same (markup-classes (nth-element (chunks document) 2)) '(bullet))
    (ensure-same (markup-classes (nth-element (chunks document) 3)) '(paragraph))))

;;; ---------------------------------------------------------------------------

(addtest (test-markdown)
  simple-bullets-with-breaks-between
  (let ((document
         (markdown 
          "this is a list

* item 1

* item 2

that's all.")))
    (ensure-same (size (chunks document)) 4)
    (ensure-same (markup-classes (nth-element (chunks document) 0)) '(paragraph))
    (ensure-same (markup-classes (nth-element (chunks document) 1)) '(bullet paragraph))
    (ensure-same (markup-classes (nth-element (chunks document) 2)) '(bullet paragraph))
    (ensure-same (markup-classes (nth-element (chunks document) 3)) '(paragraph))))

;;; ---------------------------------------------------------------------------

(addtest (test-markdown)
  simple-multiline-bullets
  (let ((document
         (markdown 
          "this is a list

* item 1
is a bullet that take
many lines
* item 2

that's all.")))
    (ensure-same (size (chunks document)) 5)))

;;; ---------------------------------------------------------------------------

(addtest (test-markdown)
  simple-multiline-bullets-with-breaks
  (let ((document
         (markdown 
          "this is a list

* item 1

  is a bullet that take
many lines

  over three paragraphs

* item 2

that's all.")))
    (ensure-same (size (chunks document)) 6)))

;;; ---------------------------------------------------------------------------

(addtest (test-markdown)
  simple-bullets-and-numbers
  (let ((document
         (markdown 
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

(addtest (test-markdown)
  simple-headers-1
  (let ((document
         (markdown 
          "Random line
Title One
========

What not

========
Just some equal signs
")))
    (ensure-same (size (chunks document)) 5)))

;;; ---------------------------------------------------------------------------

(addtest (test-markdown)
  simple-headers-2
  (let ((document
         (markdown 
          "
Title
========
Subtitle
--------

What not is 
a good start to a paragraph.

")))
    (ensure-same (size (chunks document)) 3)))

;;; ---------------------------------------------------------------------------
