(in-package #:cl-markdown-test)

(deftestsuite test-reference-links (test-snippets)
  ())

(addtest (test-reference-links)
  title-only-1
  (let ((doc
	 (cl-markdown:markdown
	  "I like [beans][]. Do you?

 [beans]: http://www.beans.com \"foo\"
" :stream :none)))
    (ensure-same (properties (first-element (link-info doc))) nil)
    (ensure-same (title (first-element (link-info doc)))
		 "foo" :test 'string=)))

(addtest (test-reference-links)
  title-only-2-a
  (let ((doc
	 (cl-markdown:markdown
	  "I like [beans][]. Do you?

 [beans]: http://www.beans.com \(foo is a bean\)
" :stream :none)))
    (ensure-same (properties (first-element (link-info doc))) nil)
    (ensure-same (title (first-element (link-info doc)))
		 "foo is a bean" :test 'string=)))

(addtest (test-reference-links)
  title-only-2-b
  (let ((doc
	 (cl-markdown:markdown
	  "I like [beans][]. Do you?

 [beans]: http://www.beans.com \"foo is a bean\"
" :stream :none)))
    (ensure-same (properties (first-element (link-info doc))) nil)
    (ensure-same (title (first-element (link-info doc)))
		 "foo is a bean" :test 'string=)))

(addtest (test-reference-links)
  properties-only-1
  (let ((doc
	 (cl-markdown:markdown
	  "I like [beans][]. Do you?

 [beans]: http://www.beans.com target new class \"external link\"
" :stream :none)))
    (ensure-same (properties (first-element (link-info doc)))
		 '((:target . "new") (:class . "external link")) :test 'equalp)
    (ensure-null (title (first-element (link-info doc))))))

(addtest (test-reference-links)
  title-and-properties-1
  (let ((doc
	 (cl-markdown:markdown
	  "I like [beans][]. Do you?

 [beans]: http://www.beans.com \"beans are the new black\" target new
" :stream :none)))
    (ensure-same (properties (first-element (link-info doc)))
		 '((:target . "new")) :test 'equalp)
    (ensure-same (title (first-element (link-info doc)))
		 "beans are the new black" :test 'string=)))

(addtest (test-reference-links)
  title-and-properties-2
  (let ((doc
	 (cl-markdown:markdown
	  "I like [beans][]. Do you?

 [beans]: http://www.beans.com \"beans are the new black\" target new class external
" :stream :none)))
    (ensure-same (properties (first-element (link-info doc)))
		 '((:target . "new") (:class . "external")) :test 'equalp)
    (ensure-same (title (first-element (link-info doc)))
		 "beans are the new black" :test 'string=)))

;; not sure how this should work
(addtest (test-reference-links
	  :expected-failure "parsing multi-line reference links")
  title-and-properties-3
  (let ((doc
	 (cl-markdown:markdown
	  "I like [beans][]. Do you?

 [beans]: http://www.beans.com \"beans are the new black\"
target new class external
" :stream :none)))
    (ensure-same (properties (first-element (link-info doc)))
		 '((:target . "new") (:class . "external")) :test 'equalp)
    (ensure-same (title (first-element (link-info doc)))
		 "beans are the new black" :test 'string=)))


(addtest (test-reference-links
	  :expected-failure "parsing multi-line reference links")
  title-on-new-line
  (let ((doc
	 (cl-markdown:markdown
	  "I like [beans][]. Do you?

 [beans]: http://www.beans.com
\"beans are the new black\"
" :stream :none)))
    (ensure-same (title (first-element (link-info doc)))
		 "beans are the new black" :test 'string=)))

