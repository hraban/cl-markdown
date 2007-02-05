(in-package #:cl-markdown-test)

#|
(run-tests :suite 'test-markdown)
|#

(deftestsuite cl-markdown-test-all () ())

(deftestsuite cl-markdown-test (cl-markdown-test-all) ())

#|
;;; ---------------------------------------------------------------------------

(addtest (cl-markdown-test)
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
    (ensure (paragraph? (nth-element (chunks document) 1)))
    (ensure (paragraph? (nth-element (chunks document) 2)))))

(addtest (cl-markdown-test)
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
    (ensure (paragraph? (nth-element (chunks document) 0)))))

;;; ---------------------------------------------------------------------------

(addtest (cl-markdown-test)
  simple-bullets-no-breaks
  (let ((document
         (markdown 
          "this is a list
* item 1
* item 2
that's all.")))
    (ensure-same (size (chunks document)) 4)
    (ensure (paragraph? (nth-element (chunks document) 0)))
    (ensure-same (markup-class (nth-element (chunks document) 1)) '(bullet))
    (ensure (paragraph? (nth-element (chunks document) 3)))))


;;; ---------------------------------------------------------------------------

(addtest (cl-markdown-test)
  simple-bullets-with-breaks
  (let ((document
         (markdown 
          "this is a list

* item 1
* item 2

that's all.")))
    (ensure-same (size (chunks document)) 4)
    (ensure (paragraph? (nth-element (chunks document) 0)))
    (ensure-same (markup-class (nth-element (chunks document) 1)) '(bullet))
    (ensure-same (markup-class (nth-element (chunks document) 2)) '(bullet))
    (ensure (paragraph? (nth-element (chunks document) 3)))))

;;; ---------------------------------------------------------------------------

(addtest (cl-markdown-test)
  simple-bullets-with-breaks-between
  (let ((document
         (markdown 
          "this is a list

* item 1

* item 2

that's all.")))
    (ensure-same (size (chunks document)) 4)
    (ensure (paragraph? (nth-element (chunks document) 0)))
    (ensure-same (markup-class (nth-element (chunks document) 1)) '(bullet paragraph))
    (ensure-same (markup-class (nth-element (chunks document) 2)) '(bullet paragraph))
    (ensure (paragraph? (nth-element (chunks document) 3)))))

;;; ---------------------------------------------------------------------------

(addtest (cl-markdown-test)
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

(addtest (cl-markdown-test)
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

(addtest (cl-markdown-test)
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

(addtest (cl-markdown-test)
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

(addtest (cl-markdown-test)
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

(addtest (cl-markdown-test)
  horizontal-rules-1
  (let ((document
         (markdown 
          "Here are some rules.
I hope you like 'em.

---
====
- - -
== == ==
_ _ _____ _ _

Did you like them?")))
    (ensure-same (size (chunks document)) 7)
    (loop for i from 1 to 5 do
          (ensure-same (markup-class (nth-element (chunks document) i))
                       '(horizontal-rule)))))

;;; ---------------------------------------------------------------------------

(addtest (cl-markdown-test)
  nested-bullets
  (let ((document
         (markdown 
          "Here are some rules.

* Item 1
    * sub-item a
    * sub-item b
        1. sub-sub-item 1 (I'm going to handle this differently than MD)
        2. sub-sub-item 2
    * sub-item c
* item 2
    * sub-item d
        * sub-sub-item 3

Did you like them?")))
    (ensure-same (size (chunks document)) 8)))

;;; ---------------------------------------------------------------------------

(addtest (cl-markdown-test)
  nested-bullets-with-paragraph
  (let ((document
         (markdown 
          "X

* Item 1
    * sub-item a

    this is part of item 1's description

* item 2")))
    (ensure-same (size (chunks document)) 4)))

;;; ---------------------------------------------------------------------------

(addtest (cl-markdown-test)
  single-blockquotes
  (let ((document
         (markdown 
          "
What did I say?

> I said 'hello'
> I said 'why'
>> I said 'because'
    
Fickle fate
")))
    (ensure-same (size (chunks document)) 4)))

;;; ---------------------------------------------------------------------------

(addtest (cl-markdown-test)
  single-blockquotes-paragraphs
  (let ((document
         (markdown 
          "
What did I say?

> I said 'hello'

> I said 'why'

>> I said 'because'
    
Fickle fate")))
    (ensure-same (size (chunks document)) 5)))

;;; ---------------------------------------------------------------------------

(addtest (cl-markdown-test)
  nested-blockquotes
  (let ((document
         (markdown 
          "
What did I say?

> I said 'hello'
>> she said 'why'
> because that's my girl
>> 'oh'
    
Fickle fate")))
    (ensure-same (size (chunks document)) 6)))

;;; ---------------------------------------------------------------------------

(addtest (cl-markdown-test)
  nested-bullets-with-blockquote
  (let ((document
         (markdown 
          "X
* Item 1
    * sub-item a

    this is part of item 1's description
    > quote this 
    >> and this
    > He said two things?
    >> * thing 1
    >> * and thing 2
    
* item 2")))
    (ensure-same (size (chunks document)) 10)))

;;; ---------------------------------------------------------------------------

(addtest (cl-markdown-test)
  paragraphs-and-code-1 
  (let ((document
         (markdown 
          "
Here is some text

    This is code
     * Don't mark it up?

OK?")))
    (ensure-same (size (chunks document)) 4)))

;;; ---------------------------------------------------------------------------

(addtest (cl-markdown-test)
  paragraphs-and-code-2 
  (let ((document
         (markdown 
          "
He gave me a __quoted__ list:

> My list
> * first thing
> * second thing
> * third thing

i smiled.")))
    (ensure-same (size (chunks document)) 6)))

;;; ---------------------------------------------------------------------------

(deftestsuite cl-markdown-test-reference-links (cl-markdown-test) 
  (document)
  (:setup (setf document 
                (markdown "
I get 10 times more traffic from [Google] [1] than from
[Yahoo] [2] or [MSN] [3].

  [1]: http://google.com/        \"Google\"
  [2]: http://search.yahoo.com/  \"Yahoo Search\"
  [3]: http://search.msn.com/    
       \"MSN Search\"
"))))

(addtest (cl-markdown-test-reference-links)
  test-document-size 
  (ensure-same (size (chunks document)) 1))

(addtest (cl-markdown-test-reference-links)
  test-link-count 
    (ensure-same (size (link-info document)) 3))

(addtest (cl-markdown-test-reference-links)
  test-link-structure 
  (ensure-same (url (item-at (link-info document) "1")) "http://google.com/")
  (ensure-same (id (item-at (link-info document) "1")) "1"))

|#