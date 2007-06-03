(in-package #:cl-markdown)

(defmacro with-stream-from-specifier ((stream stream-specifier) &body body)
  (with-gensyms (s close? output)
    `(let (,s (,close? t) ,output)
       (unwind-protect
         (setf ,output
               (prog1
                 (let (,stream)
                   (setf (values ,s ,close?) (make-stream-from-specifier ,stream-specifier)
                         ,stream ,s)
                   ,@body)
                 #+Ignore
                 (format t "~%~%~A ~A" ,close? ,s)))
         (when (and ,close? ,s)
           (awhen (close-stream-specifier ,s)
             (setf ,output it))))
       ,output))) 

(defmacro defsimple-extension (name &body body)
  "Create an extension (a function named `name`) with no arguments that 
does not depend on the markdown phase and which does not use the result.
These are handy for simple text substitutions."
  (with-gensyms (phase arguments result)
  `(defun ,name (,phase ,arguments ,result)
     (declare (ignore ,phase ,arguments ,result))
     ,@body)))

(defun %validate-defextension-arguments (arguments)
  (loop for argument in (ensure-list arguments) do
       (cond ((atom argument)
	      (when (eq (symbol-package argument) #.(find-package :keyword))
		  (error "Argument names may not be keywords and ~s is not"
			 argument)))
	     (t 
	      (unless (every (lambda (facet)
			       (member facet '(:required :keyword :whole)))
			     (rest argument))
		(error "Argument arguments facets in ~s" (rest argument)))))))

(defun %collect-arguments (arguments kind)
  (loop for argument in (ensure-list arguments) 
     when (and (consp argument)
	       (member kind (rest argument))) collect
     (first argument)))

(defun %collect-positionals (arguments)
  (loop for argument in (ensure-list arguments) 
       when (or (atom argument)
		(and (consp argument)
		     (not (member :keyword (rest argument))))) collect
     (first (ensure-list argument))))

(defmacro defextension ((name &key arguments) &body body)
  (%validate-defextension-arguments arguments)
  (bind ((keywords (%collect-arguments arguments :keyword))
	 (requires (%collect-arguments arguments :required))
	 (whole  (%collect-arguments arguments :whole))
	 (positionals (%collect-positionals arguments)))
    (assert (<= (length whole) 1)
	    nil "At most one :whole argument is allowed.")
    `(defun ,name (phase args result)
       (declare (ignorable phase args result))
       (bind (;#(args (mapcar (lambda (x) 
	      ;(ignore-errors (read-from-string x))) args))
	      ,@(loop for positional in positionals collect
		     `(,positional (pop args)))
	      ,@(loop for keyword in keywords collect
		     `(,keyword 
		       (getf args ,(intern (symbol-name keyword) :keyword)
			     nil))))
	 ,@(loop for require in requires collect
		`(assert ,require nil ,(format nil "~s is required" require)))
	 ,@body))))

 