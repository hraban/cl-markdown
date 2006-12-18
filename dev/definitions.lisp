(in-package #:cl-markdown)

(defparameter *spaces-per-tab* 4)
(defparameter *parsing-environment* nil)
(defparameter *chunk-parsing-environments*
  (make-container 'simple-associative-container))
(defparameter *spanner-parsing-environments*
  (make-container 'simple-associative-container :test #'equal))
(defparameter *horizontal-rule-count-threshold* 3)


(defparameter *default-stream* *standard-output*)
(defparameter *default-format* :html)

(defvar *output-stream* nil)
(defvar *current-indentation-level* 0)

(defparameter *current-document* nil)

(defparameter *current-format* nil)

(defparameter *render-active-functions* 
  '(table-of-contents property set-property anchor footnote footnotes
    today now))

(defparameter *parse-active-functions* 
  '(table-of-contents property set-property anchor footnote footnotes))



