(in-package #:markdown)

(define-parse-tree-synonym
    line-ends-with-two-spaces
    (:sequence 
     (:register (:sequence (:greedy-repetition 0 nil :everything)))
     #\Space #\Space :end-anchor))

(define-parse-tree-synonym
  emphasis-1 #.(cl-ppcre::parse-string "_([^_]*)_"))

(define-parse-tree-synonym 
  emphasis-2 #.(cl-ppcre::parse-string "\\*([^ ][^\\*]*)\\*"))

(define-parse-tree-synonym
  strong-1 
  (:sequence 
   (:greedy-repetition 2 2 #\_)
   (:register
    (:sequence (:greedy-repetition 0 nil (:inverted-char-class #\_))))
   (:greedy-repetition 2 2 #\_)))

(define-parse-tree-synonym 
  strong-2 
  (:sequence 
   (:greedy-repetition 2 2 #\*)
   (:register
    (:sequence (:greedy-repetition 0 nil (:inverted-char-class #\*))))
   (:greedy-repetition 2 2 #\*)))

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
  pathname-char (:char-class #\-
			     (:range #\a #\z)
			     (:range #\A #\Z)
			     (:range #\0 #\9) 
                             #\_ #\. #\: #\@ #\& #\? #\= #\+
                             #\, #\! #\/ #\~ #\* #\' #\% #\\ #\$
			     ))

(define-parse-tree-synonym
  url-pathname (:sequence (:greedy-repetition 0 nil pathname-char)))

(define-parse-tree-synonym
  url (:sequence "http://" 
                 (:register hostname) 
                 (:greedy-repetition 
                  0 1 (:sequence
		       (:greedy-repetition 0 1 #\/)
		       (:register url-pathname
				      (:greedy-repetition 
				       0 1 (:sequence #\# url-pathname)))
		       ))
                 (:negative-lookbehind (:char-class #\. #\, #\? #\!))))

(define-parse-tree-synonym
  url-no-registers
  (:sequence 
   (:greedy-repetition 0 1 (:sequence "http://" hostname)) 
   (:greedy-repetition 
    0 1 (:sequence (:greedy-repetition 0 1 #\/) url-pathname))
   (:greedy-repetition 
    0 1 (:sequence #\# url-pathname))
   (:negative-lookbehind (:char-class #\. #\, #\? #\!))))

(define-parse-tree-synonym
  bracketed (:sequence
             #\[
             (:register (:greedy-repetition 0 nil (:inverted-char-class #\[)))
             #\]))

(defparameter *escape-characters*
  "\\`*_{}[]()#.!<>")

;; FIXME - use *escape-characters* to create this parse-tree
(define-parse-tree-synonym
  valid-escape
    (:alternation 
     #\\				;backslash
     #\`				;backtick
     #\*				;asterisk
     #\_				;underscore
     #\{				;curly braces
     #\} 
     #\[				;square brackets
     #\]
     #\(				;parentheses
     #\)
     #\#				;hash mark
     #\.				;dot
     #\!				;exclamation mark
     #\<				;brackets
     #\>
     ))

(define-parse-tree-synonym
  escaped-character (:sequence #\\ (:register valid-escape)))

(define-parse-tree-synonym
    escape-kludge 
    (:sequence #\Null #\Null 
	       (:register (:greedy-repetition 0 nil 
					      (:char-class (:range #\0 #\9))))
	       #\Null #\Null))

(define-parse-tree-synonym
  link+title 
  (:sequence 
   #\(
   (:alternation
    (:sequence #\<
               (:register
		(:greedy-repetition 0 nil (:inverted-char-class #\) #\Space)))
               #\>)
    (:register (:greedy-repetition 0 nil (:inverted-char-class #\) #\Space))))
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
		  (:greedy-repetition 
		   0 1
		   (:sequence 
		    #\"
		    (:register 
		     (:greedy-repetition 0 nil :everything))
		    #\"))
		  (:register 
		   (:greedy-repetition 0 nil :everything))))))

(define-parse-tree-synonym
  extended-link-label
    (:sequence
     :start-anchor
     ;;; [reference]>
     (:greedy-repetition 0 3 :whitespace-char-class)
     bracketed
     #\> (:greedy-repetition 0 nil :whitespace-char-class)
     ;;; name
     (:register
      (:greedy-repetition 0 nil (:inverted-char-class :whitespace-char-class))) 
     (:register 
      (:greedy-repetition 0 nil :everything)) :end-anchor))

(define-parse-tree-synonym
  coded-reference-link
  (:sequence 
   #\`
   (:register
    (:sequence
     
     ;;; NO!
     ;; (:non-greedy-repetition 0 nil (:inverted-char-class #\` #\[))
                                        ; bracket
     (:sequence
      #\[ (:greedy-repetition 0 nil (:inverted-char-class #\[)) #\])
                                        ; space
     (:greedy-repetition 0 1 :whitespace-char-class)
                                        ; bracket
     (:sequence
      #\[ (:greedy-repetition 0 nil (:inverted-char-class #\[)) #\])
     
     ;;; NO!
     ;; (:non-greedy-repetition 0 nil (:inverted-char-class #\` #\]))
     
     ))
   #\`))

(define-parse-tree-synonym
  bracketed (:sequence
             #\[
             (:register (:greedy-repetition 0 nil (:inverted-char-class #\[)))
             #\]))

;;; image-link

(define-parse-tree-synonym
  inline-image (:sequence #\! bracketed link+title))

;;; image-link reference

(define-parse-tree-synonym
  inline-image (:sequence #\! bracketed link+title))

(define-parse-tree-synonym
  reference-image (:sequence 
                  #\! bracketed (:greedy-repetition 0 1 :whitespace-char-class)
                  bracketed))

