(in-package cl-markdown)

;;?? Gary King 2006-05-03: for now
(declaim (optimize (speed 0) (space 0) (debug 3)))

(defun lml2-list->tree (chunks &rest args &key (level 1))
  ;; rather inpenetrable... don't understand at the level i should...
  (let (output append? result)
    (loop for rest = chunks then (rest rest) 
          for chunk = (first rest) then (first rest) 
          while chunk 
          for new-level = (level chunk)
          for mark = (lml2-marker chunk) 

          do (setf (values output append?) (render-to-lml2 chunk))
          
           do (format t "~%C(~D): ~A, ~A" level append? chunk)
          
          when (and (= level new-level) append?) do
          (setf result `(,output ,@result))
          
          when (and (= level new-level) (not append?)) do
          (setf result `(,@output ,@result))
          
          when (< level new-level) do
          (multiple-value-bind (block remaining method)
                               (next-block rest new-level)
            (labels ((wrap-once (x)
                       (if (= x new-level)
                         (list (apply #'lml2-list->tree block :level new-level args))
                         `(,mark ,@(wrap-once (1+ x))))))
              
              (let ((inner #+Old 
                           (wrap-once level)
                           `(,mark
                             ,@(apply #'lml2-list->tree block :level (1+ level) args))))
                 (format t "~%--- ~A" method)
                (setf rest remaining)
                (ecase method
                  (:level (push-end inner (first result)))
                  (:markup (push inner result))
                  (:none (setf result inner))))))

          when (> level new-level) do (break))
    (nreverse result)))

#+Loop
(defun lml2-list->tree (chunks &rest args &key (level 1))
  ;; rather inpenetrable... don't understand at the level i should...
  (let (output append?)
    (loop for rest = chunks then (rest rest) 
          for chunk = (first rest) then (first rest) 
          while chunk 
          for new-level = (level chunk)
          for mark = (lml2-marker chunk) 

          do (setf (values output append?) (render-to-lml2 chunk))
          
          do (format t "~%C(~D): ~A, ~A" level append? chunk)
          
          when (and (= level new-level) append?) collect output
          when (and (= level new-level) (not append?)) do (break)
          
          when (< level new-level) collect
          (multiple-value-bind (block remaining method)
                               (next-block rest new-level)
            (let ((result `(,@(list mark) 
                            ,@(apply #'lml2-list->tree block :level (1+ level) args))))
              (format t "~%--- ~A" method)
              (setf rest remaining)
              (ecase method
                (:level result)
                (:markup (list result))
                (:none result))))

          when (> level new-level) do (break))))

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

