(in-package #:cl-markdown-test)

(deftestsuite test-extensions-basic (cl-markdown-test)
  ())

(deftestsuite test-properties (test-extensions-basic)
  ())

(addtest (test-properties)
  basic-get/set
  (ensure-same
   (cl-markdown::strip-whitespace
    (strip-html
     (nth-value 1
		(markdown "{set-property _my-prop_ \"hello there\"}
I say '{property _my-prop_}'." :stream nil))))
   "I say 'hello there'."))

(addtest (test-properties)
  set/get-embedded-markdown
  (let ((markdown (nth-value 1
			     (markdown "{set-property version \"**3.1**\"}
I say '{property version}'." :stream nil))))
    (ensure (search "<strong>" markdown :test #'char=))
    (ensure-same
     (cl-markdown::strip-whitespace
      (strip-html markdown))
     "I say '3.1'.")))

;; fails because the embedded {} aren't properly parsed
(addtest (test-properties)
  set/get-embedded-property
  (ensure-same
   (cl-markdown::strip-whitespace
    (strip-html
     (nth-value 1
		(markdown "{set-property version \"3.1\"}
{set-property version-name \"version-{version}\"}
I say '{property version-name}'." :stream nil))))
   "I say 'version-3.1'."))


