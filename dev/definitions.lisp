(in-package cl-markdown)

(defparameter *spaces-per-tab* 4)
(defparameter *parsing-environment* (make-instance 'parsing-environment))
(defparameter *chunk-parsing-environments*
  (make-container 'simple-associative-container))

(defvar *output-stream* nil)






(defparameter *enable-attributes* 1)
(defparameter *fn-backlink-text* "zz1337820767766393qq")

;; a template for html placeholders
(defparameter *html-placeholder-prefix* "qaodmasdkwaspemas")
(defparameter *html-placeholder* 
  (concatenate 'string *html-placeholder-prefix* "%dajkqlsmdqpakldnzsdfls"))

(defparameter *block-level-elements* 
  (let ((elements '(p div blockquote pre table
                    dl ol ul script noscript
                    form fieldset iframe math ins del)))
    (make-array (length elements) :initial-contents elements)))

;;; ---------------------------------------------------------------------------

(defparameter *pre-processors*
  '(header-pre-processor line-pre-processor
    html-block-pre-processor reference-pre-processor))

;;; ---------------------------------------------------------------------------

(defparameter *inline-patterns*
  ;; The order of the handlers matters!!! 
  '(double_backtick_pattern
    backtick_pattern
    escape_pattern
    image_link_pattern
    image_reference_pattern
    reference_pattern
    link_angled_pattern
    link_pattern
    autolink_pattern
    automail_pattern
    html_pattern
    entity_pattern
    not_strong_pattern
    strong_em_pattern
    strong_em_pattern_2
    strong_pattern
    strong_pattern_2
    emphasis_pattern
    emphasis_pattern_2
    ))

;;; ---------------------------------------------------------------------------

(defun is-block-level-p (tag)
  (or (find tag *block-level-elements*)
      (let ((tag-name (symbol-name tag))) 
        (and (char-equal (aref tag-name 0) #\h)
             (find (aref tag-name 1) "0123456789" :test #'char-equal)))))