(in-package #:cl-markdown-test)

#+(or)
(run-tests :suite 'test-headers)

(deftestsuite test-headers (test-snippets)
  ()
  (:documentation "Case 272"))

(addtest (test-headers)
  one-dash
  (check-output "asdf
-"))

(addtest (test-headers)
  two-dash
  (check-output "asdf
--"))

(addtest (test-headers)
  three-dash
  (check-output "asdf
---"))

(addtest (test-headers)
  four-dash
  (check-output "asdf
----"))

(addtest (test-headers)
  five-dash
  (check-output "asdf
-----"))

(addtest (test-headers)
  six-dash
  (check-output "asdf
------"))

