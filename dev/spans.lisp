(in-package cl-markdown)

(export '(emphasis-1 emphasis-2
          strong-1 strong-2
          backtick
          auto-link auto-mail
          html entity
          hostname-char hostname
          pathname-char url-pathname
          url url-no-registers
          bracketed link+title
          reference-link inline-link link-label))
          
(define-parse-tree-synonym 
  emphasis-1 #.(cl-ppcre::parse-string "\\*([^ ][^\\*]*)\\*"))
(define-parse-tree-synonym
  emphasis-2 #.(cl-ppcre::parse-string "_([^_]*)_"))
(define-parse-tree-synonym 
  strong-1 #.(cl-ppcre::parse-string "\\*\\*([^ ][^\\*]*)\\*\\*"))
(define-parse-tree-synonym
  strong-2 #.(cl-ppcre::parse-string "__([^_]*)__"))
(define-parse-tree-synonym 
  backtick #.(cl-ppcre::parse-string "\\`([^\\`]*)\\`"))
(define-parse-tree-synonym
  auto-link #.(cl-ppcre::parse-string "<(http://[^>]*)>"))
(define-parse-tree-synonym
  auto-mail #.(cl-ppcre::parse-string "<([^> ]*@[^> ]*)>"))
(define-parse-tree-synonym 
  html #.(cl-ppcre::parse-string "(\\<[^\\>]*\\>)"))
(define-parse-tree-synonym 
  entity #.(cl-ppcre::parse-string "(&[\\#a-zA-Z0-9]*;)"))

(define-parse-tree-synonym
  hostname-char #.(cl-ppcre::parse-string "[-a-zA-Z0-9_.]"))
(define-parse-tree-synonym
  hostname (:sequence 
            (:greedy-repetition 1 nil hostname-char)
            (:greedy-repetition 
             0 nil (:sequence #\. (:greedy-repetition 1 nil hostname-char)))))
(define-parse-tree-synonym
  pathname-char (:char-class #\- (:RANGE #\a #\z) (:RANGE #\A #\Z) (:RANGE #\0 #\9) 
                             #\_ #\. #\: #\@ #\& #\? #\= #\+
                             #\, #\! #\/ #\~ #\* #\' #\% #\\ #\$))
(define-parse-tree-synonym
  url-pathname (:sequence (:greedy-repetition 0 nil pathname-char)))

(define-parse-tree-synonym
  url (:sequence "http://" 
                 (:register hostname) 
                 (:greedy-repetition 
                  0 1 (:sequence #\/ (:register url-pathname)))
                 (:negative-lookbehind (:char-class #\. #\, #\? #\!))))

(define-parse-tree-synonym
  url-no-registers
  (:sequence 
   (:greedy-repetition 0 1 (:sequence "http://" hostname)) 
   (:greedy-repetition 
    0 1 (:sequence #\/ url-pathname))
   (:negative-lookbehind (:char-class #\. #\, #\? #\!))))

(define-parse-tree-synonym
  bracketed (:sequence
             #\[
             (:register (:greedy-repetition 0 nil (:inverted-char-class #\])))
             #\]))

(define-parse-tree-synonym
  link+title 
  (:sequence #\(
             (:register (:greedy-repetition 0 nil (:inverted-char-class #\) #\ )))
             (:greedy-repetition 
              0 1
              (:sequence (:greedy-repetition 1 nil :whitespace-char-class) #\"
                         (:register (:greedy-repetition 0 nil (:inverted-char-class #\"))) #\"))
             #\)))

(define-parse-tree-synonym
  inline-link (:sequence bracketed link+title))

(define-parse-tree-synonym
  reference-link (:sequence 
                  bracketed (:greedy-repetition 0 1 :whitespace-char-class)
                  bracketed))

(define-parse-tree-synonym
  link-label (:sequence
              (:greedy-repetition 0 3 :whitespace-char-class)
              bracketed
              #\: (:greedy-repetition 0 nil :whitespace-char-class)
              (:register url-no-registers) 
              (:greedy-repetition 
               0 1
               (:sequence 
                (:greedy-repetition 1 nil :whitespace-char-class) #\"
                (:register 
                 (:greedy-repetition 0 nil (:inverted-char-class #\"))) #\"))))


;;; image-link
;;; image-link reference

#|
(cl-ppcre::parse-string ":")
(scan '(:sequence hostname) "http://www.metabang.com")
(cl-ppcre:scan '(:sequence inline-link) "go see [this](http://foo.bar)")
(cl-ppcre:scan 
 (cl-ppcre::parse-string "hello\\s?there")
 "hellothere")
|#

;;; ---------------------------------------------------------------------------

(setf (item-at-1 *spanner-parsing-environments* 'default)
      `((,(create-scanner '(:sequence strong-2)) strong)
        (,(create-scanner '(:sequence strong-1)) strong)
        (,(create-scanner '(:sequence emphasis-2)) emphasis)
        (,(create-scanner '(:sequence emphasis-1)) emphasis)
        (,(create-scanner '(:sequence backtick)) code)
        (,(create-scanner '(:sequence auto-link)) link)
        (,(create-scanner '(:sequence auto-mail)) mail)
        ;; do before html
        (,(create-scanner '(:sequence inline-link)) inline-link)
        (,(create-scanner '(:sequence reference-link)) reference-link)
        (,(create-scanner '(:sequence entity)) entity)
        ;(,(create-scanner '(:sequence html)) html)
        ))

;;; ---------------------------------------------------------------------------

(setf (item-at-1 *spanner-parsing-environments* '(code))
      `((,(create-scanner '(:sequence html)) html)))

;;; ---------------------------------------------------------------------------

(defun spanners-for-chunk (chunk)
  (or (item-at-1 *spanner-parsing-environments* (markup-class chunk))
      (item-at-1 *spanner-parsing-environments* 'default)))

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
        (let ((lines (slot-value chunk 'lines)))
          (loop for (regex name) in (spanners-for-chunk chunk) do
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

(defmethod scan-one-span ((line cons) name regex)
  (if (process-span-in-span-p name (first line))
    `((,(first line) 
       ,@(scan-one-span (second line) name regex)
       ,@(nthcdr 2 line)))
    (list line)))

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

(defmethod scan-one-span ((line string) name regex)
  (let ((found? nil)
        (result nil)
        (last-e 0))
    (do-scans (s e gs ge regex line)
      (setf found? t
            result (append result
                           `(,@(when (plusp s) `(,(subseq line last-e s)))
                             (,name 
                              ,@(loop for s-value across gs
                                      for e-value across ge 
                                      when (and (not (null s-value))
                                                (/= s-value e-value)) collect
                                      (subseq line s-value e-value)))))
            last-e e))
    (if found?
      (values (let ((last (subseq line last-e)))
                (if (plusp (size last))
                  (append result (list last))
                  result))
              t) 
      (values (list line) nil))))

