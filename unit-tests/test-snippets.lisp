(in-package #:cl-markdown-test)

(deftestsuite test-snippets ()
  ()
  :equality-test #'string-equal)

;; test example from hhalvors@Princeton.EDU
(addtest (test-snippets)
  (ensure-same
   (nth-value 
    1 (markdown "## Common Lisp

* An item with a link [link](link.html) and some following text." :format :html :stream nil))
   "<h2>Common Lisp </h2>
<ul><li>An item with a link <a href=\"link.html\">link</a> and some following text. </li>
</ul>"))


;; test example from hhalvors@Princeton.EDU
(addtest (test-snippets)
  (ensure-same
   (nth-value 
    1 (markdown "## Common Lisp

* An item with a link [link](link.html) and some following text.
* Another item" :format :html :stream nil)
)
   "<h2>Common Lisp </h2>
<ul><li>An item with a link <a href=\"link.html\">link</a> and some following text. </li>
<li>Another item </li>
</ul>"))

;; test example from hhalvors@Princeton.EDU
(addtest (test-snippets)
  (ensure-same
   (nth-value 
    1 (markdown "## Common Lisp

* An item with a link [link](link.html) and some following text.

## A second level heading

* Another item" :format :html :stream nil))
    "<h2>Common Lisp </h2>
<ul><li>An item with a link <a href=\"link.html\">link</a> and some following text. </li>
</ul><h2>A second level heading </h2>
<ul><li>Another item </li>
</ul>"))



