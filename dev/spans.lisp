(in-package #:cl-markdown)

(defvar *current-span* nil)
          
(define-parse-tree-synonym 
  emphasis-1 #.(cl-ppcre::parse-string "\\*([^ ][^\\*]*)\\*"))
(define-parse-tree-synonym
  strong-1 #.(cl-ppcre::parse-string "_([^_]*)_"))
(define-parse-tree-synonym 
  emphasis-2 
  (:sequence 
   (:greedy-repetition 2 2 #\*)
   (:register
    (:sequence (:greedy-repetition 0 nil (:inverted-char-class #\*))))
   (:greedy-repetition 2 2 #\*)))
(define-parse-tree-synonym
  strong-2 
  (:sequence 
   (:greedy-repetition 2 2 #\_)
   (:register
    (:sequence (:greedy-repetition 0 nil (:inverted-char-class #\_))))
   (:greedy-repetition 2 2 #\_)))
(define-parse-tree-synonym
  strong-em-1
  (:sequence 
   (:greedy-repetition 3 3 #\_)
   (:register
    (:sequence (:greedy-repetition 0 nil (:inverted-char-class #\_))))
   (:greedy-repetition 3 3 #\_)))
(define-parse-tree-synonym
  strong-em-2 
  (:sequence 
   (:greedy-repetition 3 3 #\*)
   (:register
    (:sequence (:greedy-repetition 0 nil (:inverted-char-class #\*))))
   (:greedy-repetition 3 3 #\*)))
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
             (:register (:greedy-repetition 0 nil :everything))
             #\]))

(define-parse-tree-synonym
  escaped-character (:sequence
                     #\\
                     (:register
                      (:alternation 
                       #\\               ;backslash
                       #\`               ;backtick
                       #\*               ;asterisk
                       #\_               ;underscore
                       #\{               ;curly braces
                       #\} 
                       #\[               ;square brackets
                       #\]
                       #\(               ;parentheses
                       #\)
                       #\#               ;hash mark
                       #\.               ;dot
                       #\!               ;exclamation mark
                       ))))

#+Old
(define-parse-tree-synonym
  bracketed (:sequence
             #\[
             (:register (:greedy-repetition 0 nil (:inverted-char-class #\])))
             #\]))

(define-parse-tree-synonym
  link+title 
  (:sequence 
   #\(
   (:alternation
    (:sequence #\<
               (:register (:greedy-repetition 0 nil (:inverted-char-class #\) #\ )))
               #\>)
    (:register (:greedy-repetition 0 nil (:inverted-char-class #\) #\ ))))
                                        ; title
   (:greedy-repetition 
    0 1
    (:sequence 
     (:greedy-repetition 1 nil :whitespace-char-class)
     (:alternation #\' #\" #\() 
     (:register (:greedy-repetition 0 nil :everything))
     (:alternation #\' #\" #\))))
   #\)))

(define-parse-tree-synonym
  inline-link (:sequence bracketed link+title))

(define-parse-tree-synonym
  reference-link (:sequence 
                  bracketed (:greedy-repetition 0 1 :whitespace-char-class)
                  bracketed))

(define-parse-tree-synonym
  link-label (:sequence
              :start-anchor
              (:greedy-repetition 0 3 :whitespace-char-class)
              bracketed
              #\: (:greedy-repetition 0 nil :whitespace-char-class)
              (:register url-no-registers) 
              (:greedy-repetition 
               0 1
               (:sequence 
                (:greedy-repetition 1 nil :whitespace-char-class)
                #\"
                (:register 
                 (:greedy-repetition 0 nil :everything))
                #\"))))


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
      `((,(create-scanner '(:sequence escaped-character)) escaped-character)
        (,(create-scanner '(:sequence strong-em-1)) strong-em)
        (,(create-scanner '(:sequence strong-em-2)) strong-em)
        
        (,(create-scanner '(:sequence strong-2)) strong)
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

(defmethod scan-one-span ((line string) name regex)
  (when (process-span-in-span-p *current-span* name)
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
      (when found?
        (return-from scan-one-span
          (values (let ((last (subseq line last-e)))
                    (if (plusp (size last))
                      (append result (list last))
                      result))
                  t)))))
  (values (list line) nil))

