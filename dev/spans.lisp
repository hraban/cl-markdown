(in-package #:cl-markdown)

(defvar *current-span* nil)
          
;;; ---------------------------------------------------------------------------

(setf (item-at-1 *spanner-parsing-environments* 'default)
      `((,(create-scanner '(:sequence escaped-character)) escaped-character)
        
        ;; do early
        (,(create-scanner '(:sequence backtick)) code)

        (,(create-scanner '(:sequence strong-em-1)) strong-em)
        (,(create-scanner '(:sequence strong-em-2)) strong-em)
        
        (,(create-scanner '(:sequence strong-2)) strong)
        (,(create-scanner '(:sequence strong-1)) strong)
        (,(create-scanner '(:sequence emphasis-2)) emphasis)
        (,(create-scanner '(:sequence emphasis-1)) emphasis)
        (,(create-scanner '(:sequence auto-link)) link)
        (,(create-scanner '(:sequence auto-mail)) mail)
        ;; do before html
        (,(create-scanner '(:sequence inline-link)) inline-link)
        (,(create-scanner '(:sequence reference-link)) reference-link)
        (,(create-scanner '(:sequence entity)) entity)
        (,(create-scanner '(:sequence html)) html)
        ))

;;; ---------------------------------------------------------------------------

(setf (item-at-1 *spanner-parsing-environments* '(code))
      `((,(create-scanner '(:sequence html)) html)))

;;; ---------------------------------------------------------------------------

(defun scanners-for-chunk (chunk)
  (acond ((item-at-1 *spanner-parsing-environments* (markup-class chunk))
          (values it (markup-class chunk)))
         (t
          (values (item-at-1 *spanner-parsing-environments* 'default) nil))))

;;; ---------------------------------------------------------------------------

(defmethod handle-spans ((document document))
  (iterate-elements
   (chunks document)
   (lambda (chunk)
     (handle-spans chunk)))
  document)

;;; ---------------------------------------------------------------------------

(defmethod handle-spans ((chunk chunk)) 
  (setf (slot-value chunk 'lines)
        (bind ((lines (slot-value chunk 'lines))
               ((values scanners kind) (scanners-for-chunk chunk))
               (*current-span* kind))
          (loop for (regex name) in scanners do
                (setf lines
                      (let ((result nil))
                        (iterate-elements
                         lines
                         (lambda (line) 
                           (setf result 
                                 (append result (scan-one-span line name regex)))))
                        result)))
          lines))
  chunk)

;;; ---------------------------------------------------------------------------

(defmethod scan-one-span ((line (eql nil)) name regex)
  (list ""))

;;; ---------------------------------------------------------------------------

(defmethod scan-one-span ((line cons) name regex)
  (if (process-span-in-span-p name (first line))
    `((,(first line) 
       ,@(let ((*current-span* (first line)))
           (scan-one-span (second line) name regex))
       ,@(nthcdr 2 line)))
    (list line)))

;;; ---------------------------------------------------------------------------

(defmethod process-span-in-span-p ((span-1 (eql nil)) (span-2 (eql 'html))) 
  (values nil))

;;; ---------------------------------------------------------------------------

(defmethod process-span-in-span-p ((span-1 t) (span-2 t)) 
  (values t))

;;; ---------------------------------------------------------------------------

(defmethod process-span-in-span-p ((span-1 (eql 'link)) (span-2 (eql 'code))) 
  (values nil))

;;; ---------------------------------------------------------------------------


(defmethod process-span-in-span-p ((span-1 (eql 'html)) (span-2 t)) 
  (values nil))

;;; ---------------------------------------------------------------------------

(defmethod process-span-in-span-p ((span-1 (eql 'html)) (span-2 (eql 'code))) 
  (values t))

;;; ---------------------------------------------------------------------------

(defmethod process-span-in-span-p ((span-1 (eql 'code)) (span-2 t)) 
  (values nil))

;;; ---------------------------------------------------------------------------

(defmethod scan-one-span ((line string) name regex)
  (when (process-span-in-span-p *current-span* name)
    (let ((found? nil)
          (result nil)
          (last-e 0))
      (do-scans (s e gs ge regex line)
        (let ((registers (loop for s-value across gs
                               for e-value across ge 
                               when (and (not (null s-value))
                                         (/= s-value e-value)) collect
                               (subseq line s-value e-value))))
          (setf registers (process-span name registers))
          (setf found? t
                result (append result
                               `(,@(when (plusp s) `(,(subseq line last-e s)))
                                 (,name ,@registers)))
                last-e e)))
      (when found?
        (return-from scan-one-span
          (values (let ((last (subseq line last-e)))
                    (if (plusp (size last))
                      (append result (list last))
                      result))
                  t)))))
  (values (list line) nil))

 