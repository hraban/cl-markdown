(in-package #:cl-markdown-test)

(deftestsuite brackets-with-empty-lines (test-bracket-processing)
  ())

(addtest (brackets-with-empty-lines)
  linefeed-in-bracket
  (ensure (search "guide for test 3.0"
		  (nth-value 1
			     (markdown "{set-property test \"3.0\"}

This is the guide for test {property
test}. It rocks." :stream nil))
		  :test 'char=)))

(addtest (brackets-with-empty-lines)
  no-linefeeds
  (ensure (search "footnoteBacklink"
		  (nth-value 1 (markdown "
Hi there
this is a footnote{footnote \"Actualy, this is\"}. Nice.

{footnotes}" :stream nil)) :test #'char=)))

(addtest (brackets-with-empty-lines)
  one-linefeed
  (ensure (search "footnoteBacklink"
		  (nth-value 1 (markdown "
Hi there
this is a footnote{footnote \"Actualy,
this is\"}. Nice.

{footnotes}" :stream nil)) :test #'char=)))


(addtest (brackets-with-empty-lines)
  two-linefeeds
  (ensure (search "footnoteBacklink"
		  (nth-value 1 (markdown "
Hi there
this is a footnote{footnote \"Actualy,

this is\"}. Nice.

{footnotes}" :stream nil)) :test #'char=)))
