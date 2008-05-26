(in-package #:cl-markdown)

(defmacro defsimple-extension (name &body body)
  "Create an extension (a function named `name`) with no arguments that 
does not depend on the markdown phase and which does not use the result.
These are handy for simple text substitutions."
  (with-gensyms (phase arguments result)
  `(progn
     (pushnew (list ',name t) *extensions* :key #'car)
     (defun ,name (,phase ,arguments ,result)
       (declare (ignore ,phase ,arguments ,result))
       ,@body)
     ,@(%import/export-symbol name))))

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
		(error "Invalid argument facets in ~s" (rest argument)))))))

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

(defparameter *extensions* nil)

(defmacro defextension ((name &key arguments (insertp nil)) &body body)
  (%validate-defextension-arguments arguments)
  (bind ((keywords (%collect-arguments arguments :keyword))
	 (requires (%collect-arguments arguments :required))
	 (whole  (%collect-arguments arguments :whole))
	 (positionals (%collect-positionals arguments)))
    (assert (<= (length whole) 1)
	    nil "At most one :whole argument is allowed.")
    (assert (null (intersection whole keywords))
	    nil "Keyword arguments cannot be wholes")
    `(progn
       (setf *extensions* (remove ',name *extensions* :key #'first))
       (push (list ',name ,insertp) *extensions*)
       (defun ,name (phase args result)
	 (declare (ignorable phase args result))
	 (bind (,@(loop for positional in positionals
		     unless (member positional whole) collect
		       `(,positional (pop args)))
		  ,@(loop for keyword in keywords collect
			 `(,keyword 
			   (getf args ,(intern (symbol-name keyword) :keyword)
				 nil)))
		  ,@(when whole
			  `((,(first whole)
			      ;; remove keywords from args
			      (progn
				,@(loop for keyword in keywords collect
				       `(,keyword 
					 (remf args
					       ,(intern (symbol-name keyword) :keyword))))
				(if (length-1-list-p args) (first args) args))))))
	   ,@(loop for require in requires collect
		  `(assert ,require nil ,(format nil "~s is required" require)))
	   ,@body
	   ,@(unless insertp nil)))
       ,@(%import/export-symbol name))))

(defun %import/export-symbol (name)
  `((eval-when (:compile-toplevel :load-toplevel :execute)
      (import ',name ,(find-package :cl-markdown-user))
      (export ',name ,(find-package :cl-markdown-user)))))
 