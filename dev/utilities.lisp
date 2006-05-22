(in-package #:cl-markdown)

;;?? Gary King 2006-05-03: for now
(declaim (optimize (speed 0) (space 0) (debug 3)))

(defun next-block (chunks level)
  (let* ((pos-level (position-if 
                     (lambda (other)
                       (let ((other-level (level other))) 
                         (< other-level level)))
                     (rest chunks)))
         (pos-markup (position-if 
                      (lambda (other)
                        (let ((other-level (level other))) 
                          (not (and (= other-level level)
                                    (samep (markup-class other) 
                                           (markup-class (first chunks)))))))
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
          ((and pos-level pos-markup (= pos-level pos-markup))
           (setf deeper (subseq chunks 0 (1+ pos-level))
                 chunks (nthcdr pos-level chunks)
                 pos-style :level))
          (t
           ;; nothing found, take the rest
           (setf deeper chunks 
                 chunks nil
                 pos-style :none)))
    (values deeper chunks pos-style)))

