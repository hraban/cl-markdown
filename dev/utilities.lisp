(in-package #:cl-markdown)

;;?? Gary King 2006-05-03: for now
(declaim (optimize (speed 0) (space 0) (debug 3)))

(defun lml2-list->tree (chunks &rest args &key (level nil))
  (unless level
    (setf level (or (and (first chunks) (level (first chunks))) 0)))
  
  (labels ((do-it (chunks level &rest args)
             
             ;;?? rather inpenetrable... don't understand at the level I should...
             (apply-mark 
              (lml2-marker (first chunks))
              (let (output append? result)
                (loop for rest = chunks then (rest rest) 
                      for chunk = (first rest) then (first rest) 
                      while chunk 
                      for new-level = (level chunk)
                      
                      do (setf (values output append?) (render-to-lml2 chunk))
                      
                     ;  do (format t "~%C(~D): ~A, ~A" level append? chunk)
                      
                      when (and (= level new-level) append?) do
                      (setf result `(,output ,@result))
                      
                      when (and (= level new-level) (not append?)) do
                      (setf result `(,@output ,@result))
                      
                      when (< level new-level) do
                      (multiple-value-bind (block remaining method)
                                           (next-block rest new-level)
                        (let ((inner (apply #'do-it block (1+ level) args)))
                          ; (format t "~%--- ~A" method)
                          (setf rest remaining)
                          (ecase method
                            (:level (if (listp (first result))
                                      (push-end inner (first result))
                                      (push inner result)))
                            (:markup (push inner result))
                            (:none 
                             (setf result `(,inner ,@result))))))
                      
                      when (> level new-level) do 
                      (warn "unexpected chunk level"))
                (reverse result)))))
    (apply #'do-it chunks level args)))

;;; ---------------------------------------------------------------------------

(defun next-block (chunks level)
  (let* ((pos-level (position-if 
                     (lambda (other)
                       (let ((other-level (level other))) 
                         (< other-level level)))
                     (rest chunks)))
         (pos-markup (position-if 
                      (lambda (other)
                        (let ((other-level (level other))) 
                          (and (= other-level level)
                               (samep (markup-class other) 
                                      (markup-class (first chunks))))))
                      (rest chunks)))
         pos-style
         deeper)
    ; (format t "~%POS: ~A, ~A" pos-level pos-markup)
    
    ;; remember that there will be another call to rest 
    (cond ((or (and pos-level pos-markup (< pos-level pos-markup))
               (and pos-level (not pos-markup)))
           (setf deeper (subseq chunks 0 (1+ pos-level))
                 chunks (nthcdr pos-level chunks)
                 pos-style :level))
          ((or (and pos-level pos-markup (> pos-level pos-markup))
               (and (not pos-level) pos-markup))
           (setf deeper (subseq chunks 0 (+ pos-markup 2))
                 chunks (nthcdr (1+ pos-markup) chunks)
                 pos-style :markup))
          ;; = case not possible
          #+Ignore (and pos-level pos-markup (= pos-level pos-markup))
          (t
           ;; nothing found, take the singleton
           (setf deeper (list (first chunks)) 
                 chunks (rest chunks)
                 pos-style :none)))
    (values deeper chunks pos-style)))

;;; ---------------------------------------------------------------------------

(defun apply-mark (mark rest)
  (cond ((null mark) rest)
        ((consp mark) 
         (if (length-1-list-p mark)
           `(,(first mark) ,@(apply-mark (rest mark) rest))
           `(,(first mark) (,@(apply-mark (rest mark) rest)))))
        (t
         (error "unhandled case"))))



