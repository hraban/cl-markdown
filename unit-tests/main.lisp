(in-package #:cl-markdown-test)

(defun test-all ()
  (let ((lift:*current-asdf-system-name* "cl-markdown"))
    (lift:run-tests :config :generic))
  (unless (eq :success (lift:last-test-status))
    (error "Unexpected error or failure in test suite")))
