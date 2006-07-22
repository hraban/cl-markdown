(in-package #:cl-markdown)

(defgeneric process-span (name registers)
  (:documentation "Called during span processing on each match of name in the
document. Registers a list of the registers captured by names regular expression.
Returns a possibly new set of registers.")
  (:method ((name t) (registers t))
           (values registers)))
