(in-package #:cl-markdown-test)

(deftestsuite test-anchors (test-snippets)
  ()
  (:dynamic-variables
   (cl-markdown::*default-properties*
   '((:omit-initial-paragraph t)
     (:omit-final-paragraph t)))))

(addtest (test-anchors)
  just-a-name
  (check-html-output
"@(foo)"
"<a name=\"foo\"></a>"))

(addtest (test-anchors)
  name-with-text
  (check-html-output
"@[bar](foo)"
"<a name=\"foo\">bar</a>"))

#|
(markdown
"
hi
@(foo)
@[bar](foo)
" :format :html)

(markdown "hi @(foo)" :stream nil)
(shell-tidy "hi <a name=\"foo\"></a>")
(markdown "@(foo)"
	  :properties '((:omit-initial-paragraph t)
			(:omit-final-paragraph t)))
(markdown "@[bar](foo)")
|#