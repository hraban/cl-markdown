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
  three-dash-with-whitespace
  (check-output "asdf
---   "))

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

(addtest (test-headers)
  starts-with-dashes
  (check-output "asdf
-- it's the bomb"))

(addtest (test-headers)
  really-an-hr
  (check-output "
---"))
