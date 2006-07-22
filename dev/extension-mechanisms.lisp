(in-package #:cl-markdown)

#|
extensions should have a unique name and a priority (as should the built-ins)
|#

;;?? only add once
(defun add-extension (extension)
  (iterate-key-value 
   *spanner-parsing-environments*
   (lambda (key value)
     (setf (item-at *spanner-parsing-environments* key)
           (append value
                   (list extension))))))




#|
(markdown "Hello {user-name :format :long}, how are you. Go {{here}}." :format :none)
==> '("Hello "
      (EVAL "user-name :format :long")
      ", how are you. Go "
      (MARKDOWN::WIKI-LINK "here") 
      ". ")

(markdown "Today is {today}. It is {now}; not then.")
|#

(defparameter *render-active-functions* 
  '(table-of-contents property set-property anchor))

(defparameter *parse-active-functions* 
  '(table-of-contents property set-property anchor))

(define-parse-tree-synonym
  wiki-link (:sequence
             #\{ #\{
             (:register (:greedy-repetition 0 nil (:inverted-char-class #\})))
             #\} #\}))

(define-parse-tree-synonym
  eval (:sequence
        #\{ 
        (:register (:greedy-repetition 0 nil (:inverted-char-class #\})))
        #\}))

;; should only happen once! (but need names to do this correctly)
(eval-when (:load-toplevel :execute)
  (add-extension (list (create-scanner '(:sequence wiki-link)) 'wiki-link))
  (add-extension (list (create-scanner '(:sequence eval)) 'eval)))

(defmethod render-span-to-html ((code (eql 'eval)) body)
  ;;?? parse out commands and arguments (deal with quoting, etc)
  (bind (((command arguments result nil #+(or) processed?) body)
         (result
          (cond ((and (member command *render-active-functions*)
                      (fboundp command))
                 (funcall command :render arguments (ensure-list result)))
                (t
                 (warn "Inactive or undefined CL-Markdown function ~s" command)
                 nil))))
    (when result
      (output-html (list result)))))

(defmethod process-span ((name (eql 'eval)) registers)
  ;;; only register contains the command and all its arguments as one big string
  ;; we tokenize it and make sure the command exists and, if it is 'active'
  ;; during parsing, we call it for effect.
  (bind (((command &rest arguments) (tokenize-string (first registers)))
         (command (read-from-string command))
         ((values result processed?)
          (when (member command *parse-active-functions*)
            (if (fboundp command)
              (values (funcall command :parse arguments nil) t)
              (warn "Undefined CL-Markdown parse active function ~s" command)))))
    `(,command ,arguments ,result ,processed?)))
  
  
  