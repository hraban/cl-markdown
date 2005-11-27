(in-package cl-markdown)

(defparameter *enable-attributes* 1)
(defparameter *tab-length* 4)
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

(defun is-block-level-p (tag)
  (or (find tag *block-level-elements*)
      (let ((tag-name (symbol-name tag))) 
        (and (char-equal (aref tag-name 0) #\h)
             (find (aref tag-name 1) "0123456789" :test #'char-equal)))))