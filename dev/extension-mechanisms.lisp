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
       (setf (item-at *spanner-parsing-environments* key)
             (append (list extension) value)
	     #+(or)
	     (append value (list extension)))))))


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
  (add-extension (list (create-scanner '(:sequence eval)) 'eval)
                 :filter (lambda (key) (not (equal key '(code))))))

;;; ---------------------------------------------------------------------------

(defmethod render-span-to-html ((code (eql 'eval)) body)
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
    #+(or)
    (format t "~&~s: ~s ~s ~s" 
	    command (fboundp command)
	    (member command *render-active-functions*) result)
    (when result
      (output-html (list result)))))

;;; ---------------------------------------------------------------------------

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
              (warn "Undefined CL-Markdown parse active function ~s" 
		    command)))))
    #+(or)
    (format t "~&~a: ~a ~a ~a" 
	    command (fboundp command)
	    (member command *parse-active-functions*) result)
    `(,command ,arguments ,result ,processed?)))

(defmethod process-span-in-span-p ((span-1 t) (span-2 (eql 'eval))) 
  (values nil))

  
  
  