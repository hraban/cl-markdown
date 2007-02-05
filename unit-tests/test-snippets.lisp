(in-package #:cl-markdown-test)

(defun tidy (source)
  (bind (((values result error status)
	  (shell-command 
	   (format nil "tidy --show-body-only 1 --quiet 1 ~
                 --show-warnings 0")
	   :input source)))
    (values result error status)))

(defun shell-markdown (source)
  (bind (((values result error status)
	  (shell-command 
	   (format nil "markdown")
	   :input source)))
    (values result error status)))

(deftestsuite test-snippets ()
  ()
  :equality-test #'string-equal
  (:function 
   (check-html-output
    (source html)
    (ensure-same 
     (tidy
      (nth-value 
       1 (markdown source :stream nil :format :html)))
     (tidy html) :test 'samep)))
  (:function 
   (check-output 
    (source)
    (ensure-same (tidy
		  (nth-value 
		   1 (markdown source :stream nil :format :html)))
		 (tidy (shell-markdown source)) 
		 :test 'samep))))

;; test example from hhalvors@Princeton.EDU
(addtest (test-snippets)
  (check-output 
   "## Common Lisp

* An item with a link [link](link.html) and some following text."))

;; test example from hhalvors@Princeton.EDU
(addtest (test-snippets)
  (check-output
   "## Common Lisp

* An item with a link [link](link.html) and some following text.
* Another item"))

;; test example from hhalvors@Princeton.EDU
(addtest (test-snippets)
  (check-output
   "## Common Lisp

* An item with a link [link](link.html) and some following text.

## A second level heading

* Another item"))

(addtest (test-snippets)
  (check-output
   "* A first list item
with a hard return
* A second list item
"))

(addtest (test-snippets)
  (check-output
   "* first line

second line"))

(addtest (test-snippets)
  (check-output
   "* first line

* second line"))

(addtest (test-snippets)
  (check-output
   "* first line
* second line"))

(addtest (test-snippets)
  (check-output
   "* first line
second line"))

