(in-package #:cl-markdown-test)

(deftestsuite test-spans (cl-markdown-test-all) ())

(addtest (test-spans)
  test-1-replacement
  (ensure-same
   (scan-lines-with-scanners
    (list "Can you say **strong**?")
    `((,(create-scanner '(:sequence strong-1)) strong)
      (,(create-scanner '(:sequence strong-2)) strong)))
   '("Can you say " (STRONG "strong") "?")
   :test 'equalp))

(addtest (test-spans)
  test-2-replacement
  (ensure-same
   (scan-lines-with-scanners
    (list "Can you say **strong**? I can **say** strong!")
    `((,(create-scanner '(:sequence strong-1)) strong)
      (,(create-scanner '(:sequence strong-2)) strong)))
   '("Can you say " (STRONG "strong")
     "? I can " (STRONG "say") " strong!")
   :test 'equalp))

;;; ---------------------------------------------------------------------------

(addtest (test-spans)
  test-inline-link
  (ensure-same
   (scan-lines-with-scanners
    (list "This is [Google](http://google.com/). OK")
    `((,(create-scanner '(:sequence inline-link)) inline-link)))
   '("This is " (INLINE-LINK "Google" "http://google.com/" NIL) ". OK")
   :test 'equalp))

;;; ---------------------------------------------------------------------------

(addtest (test-spans)
  test-inline-link-with-title
  (ensure-same
   (scan-lines-with-scanners
    (list "This is [Google](http://google.com/ \"A nice title\"). OK")
    `((,(create-scanner '(:sequence inline-link)) inline-link)))
   '("This is " (INLINE-LINK "Google" "http://google.com/" "A nice title") ". OK")
   :test 'equalp))

;;; ---------------------------------------------------------------------------

(addtest (test-spans)
  test-reference-link-1
  (ensure-same
   (scan-lines-with-scanners
    (list "This is [Google][Foo]. OK")
    `((,(create-scanner '(:sequence reference-link)) reference-link)))
   '("This is " (reference-link "Google" "Foo") ". OK")
   :test 'equalp))

(addtest (test-spans)
  test-reference-link-2
  (ensure-same
   (scan-lines-with-scanners
    (list "This is [Google] [Foo]. OK")
    `((,(create-scanner '(:sequence reference-link)) reference-link)))
   '("This is " (reference-link "Google" "Foo") ". OK")
   :test 'equalp))

(addtest (test-spans)
  test-reference-link-implicit
  (ensure-same
   (scan-lines-with-scanners
    (list "This is [Google][]. OK")
    `((,(create-scanner '(:sequence reference-link)) reference-link)))
   '("This is " (reference-link "Google" "") ". OK")
   :test 'equalp))

(addtest (test-spans)
  test-reference-link-with-spaces
  (ensure-same
   (scan-lines-with-scanners
    (list "This is [Daring Fireball][]. OK")
    `((,(create-scanner '(:sequence reference-link)) reference-link)))
   '("This is " (reference-link "Daring Fireball" "") ". OK")
   :test 'equalp))


(deftestsuite test-strong-2 (test-spans)
  ((scanner (create-scanner
             '(:sequence
               (:greedy-repetition 2 2 #\*)
               (:register
                (:sequence (:greedy-repetition 0 nil (:inverted-char-class #\*))))
               (:greedy-repetition 2 2 #\*))))))

(addtest (test-strong-2)
  test-1
  (ensure (scan scanner "**hello**")))

(addtest (test-strong-2)
  test-1
  (ensure (not (scan scanner "**hello *"))))

(addtest (test-strong-2)
  test-1
  (ensure-same (scan scanner "***hello***") (values 1 10 #(3) #(8)) :test #'equalp))

(addtest (test-strong-2)
  test-1
  (ensure-same (scan scanner "*** hello there ***") (values 1 18 #(3) #(16)) :test #'equalp))


