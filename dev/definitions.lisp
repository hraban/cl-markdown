(in-package cl-markdown)

(defparameter *spaces-per-tab* 4)
(defparameter *parsing-environment* nil)
(defparameter *chunk-parsing-environments*
  (make-container 'simple-associative-container))
(defparameter *horizontal-rule-count-threshold* 3)


(defvar *output-stream* nil)
(defvar *current-indentation-level* 0)


