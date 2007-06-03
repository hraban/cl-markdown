(in-package #:cl-markdown)

#|
extensions should have a unique name and a priority (as should the built-ins)
|#

;;?? only add once
(defun add-extension (extension &key (filter (constantly t)))
  (iterate-key-value 
   *spanner-parsing-environments*
   (lambda (key value)
     (when (funcall filter key)
       (insert-new-item value extension)))))
	

#|
(markdown "Hello {user-name :format :long}, how are you. Go {{here}}." :format :none)
==> '("Hello "
      (EVAL "user-name :format :long")
      ", how are you. Go "
      (MARKDOWN::WIKI-LINK "here") 
      ". ")

(let ((*render-active-functions* 
       (append '(today now) *render-active-functions*)))
  (markdown "Today is {today}. It is {now}." 
            :format :html :stream t))

|#

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
  #+(or)
  (add-extension (list (create-scanner '(:sequence wiki-link)) 'wiki-link)
                 :filter (lambda (key) (not (equal key '(code)))))
  (add-extension 
   (make-markdown-scanner
     :regex (create-scanner '(:sequence eval))
     :name 'eval
     :priority 1.5)
   :filter (lambda (key) (not (equal key '(code))))))

(defmethod render-span-to-html ((code (eql 'eval)) body encoding-method)
  (declare (ignore encoding-method))
  ;;?? parse out commands and arguments (deal with quoting, etc)
  (bind (((command arguments result nil #+(or) processed?) body)
         (result
          (cond ((and (member command *render-active-functions*)
                      (fboundp command))
                 (funcall command :render arguments (ensure-list result)))
		((and (member command *render-active-functions*)
		      (not (fboundp command)))
                 (warn "Undefined CL-Markdown function ~s" command))
                (t
                 nil))))
    (when result
      (output-html (list result)))))

(defmethod process-span ((name (eql 'eval)) registers)
  ;;; the one register contains the command and all its arguments as one 
  ;; big string we tokenize it and make sure the command exists and, if 
  ;; it is 'active' during parsing, we call it for effect.
  (bind (((command &rest arguments) 
	  (%pull-arguments-from-string (first registers)))
         ((values result processed?)
          (when (member command *parse-active-functions*)
            (if (fboundp command)
              (values (funcall command :parse arguments nil) t)
              (warn "Undefined CL-Markdown parse active function ~s" 
		    command)))))
    #+(or)
    (format t "~&~a: ~a ~a ~a" 
	    command (fboundp command)
	    (member command *parse-active-functions*) result)
    `(,command ,arguments ,result ,processed?)))

(defmethod process-span-in-span-p ((span-1 t) (span-2 (eql 'eval))) 
  (values nil))

(defun %pull-arguments-from-string (string)
  (let ((start 0)
	(done (load-time-value (list :eof)))
	(result nil))
    (loop collect
	 (multiple-value-bind (value new-start)
	     (ignore-errors (read-from-string string nil done :start start))
	   (when (eq value done)
	     (return))
	   (cond ((and new-start (numberp new-start))
		  (setf start new-start)
		  (push value result))
		 (t
		  (incf start)))))
    (nreverse result)))
  
;;;;;

#| Another extension mechanism

|#

(defmethod generate-link-output-for-kind 
    ((kind (eql :glossary)) (link-info extended-link-info) text)  
  (declare (ignore text)))


