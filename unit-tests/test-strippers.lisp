(in-package #:cl-markdown-test)

(deftestsuite test-strippers ()
  ())

(deftestsuite test-one-tab-stripper (test-strippers)
  ()
  (:equality-test 'samep))

(addtest
  test-spaces-1
  (ensure-same (one-tab-stripper "    hello") (values "hello" t)))

(addtest
  test-tabs-1
  (ensure-same (one-tab-stripper (concatenate 'string (string #\Tab) "hello"))
               (values "hello" t)))

(addtest
  test-tabs-2
  (ensure-same (one-tab-stripper (concatenate 'string (string #\Tab) "  hello"))
               (values "  hello" t)))

(addtest
  test-spaces-2
  (ensure-same (one-tab-stripper "  hello") (values "  hello" nil)))