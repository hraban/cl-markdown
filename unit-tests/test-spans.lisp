(in-package cl-markdown-test)

(deftestsuite test-spans (test-all-markdown) ())

(addtest (test-spans)
  test-1-replacement
  (ensure-same
   (handle-spans 
    (list "Can you say **strong**?")
    `((,(create-scanner '(:sequence strong-1)) strong)
      (,(create-scanner '(:sequence strong-2)) strong)))
   '("Can you say " (STRONG "strong") "?")
   :test 'equalp))

(addtest (test-spans)
  test-2-replacement
  (ensure-same
   (handle-spans 
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
   (handle-spans 
    (list "This is [Google](http://google.com/). OK")
    `((,(create-scanner '(:sequence inline-link)) inline-link)))
   '("This is " (INLINE-LINK "Google" "http://google.com/" NIL) ". OK")
   :test 'equalp))

;;; ---------------------------------------------------------------------------

(addtest (test-spans)
  test-inline-link-with-title
  (ensure-same
   (handle-spans 
    (list "This is [Google](http://google.com/ \"A nice title\"). OK")
    `((,(create-scanner '(:sequence inline-link)) inline-link)))
   '("This is " (INLINE-LINK "Google" "http://google.com/" "A nice title") ". OK")
   :test 'equalp))

;;; ---------------------------------------------------------------------------

(addtest (test-spans)
  test-reference-link-1
  (ensure-same
   (handle-spans 
    (list "This is [Google][Foo]. OK")
    `((,(create-scanner '(:sequence reference-link)) reference-link)))
   '("This is " (reference-link "Google" "Foo") ". OK")
   :test 'equalp))

(addtest (test-spans)
  test-reference-link-2
  (ensure-same
   (handle-spans 
    (list "This is [Google] [Foo]. OK")
    `((,(create-scanner '(:sequence reference-link)) reference-link)))
   '("This is " (reference-link "Google" "Foo") ". OK")
   :test 'equalp))

(addtest (test-spans)
  test-reference-link-implicit
  (ensure-same
   (handle-spans 
    (list "This is [Google][]. OK")
    `((,(create-scanner '(:sequence reference-link)) reference-link)))
   '("This is " (reference-link "Google" "") ". OK")
   :test 'equalp))

(addtest (test-spans)
  test-reference-link-with-spaces
  (ensure-same
   (handle-spans 
    (list "This is [Daring Fireball][]. OK")
    `((,(create-scanner '(:sequence reference-link)) reference-link)))
   '("This is " (reference-link "Daring Fireball" "") ". OK")
   :test 'equalp))