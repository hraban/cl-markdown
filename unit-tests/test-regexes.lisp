(in-package #:cl-markdown-test)

(deftestsuite test-regexes (cl-markdown-test-all) ())

(deftestsuite test-url (test-regexes) ())
(addtest (test-url)
  test-1
  (ensure-same 
   (scan-to-strings
    '(:sequence url) "My page is at http://www.metabang.com/~gwking/public.")
   (values "http://www.metabang.com/~gwking/public"
           #("www.metabang.com" "~gwking/public"))
   :test 'equalp))

;;; ---------------------------------------------------------------------------
          
(deftestsuite test-link-label (test-regexes) ())
(addtest (test-link-label)
  test-link
  (bind (((values nil registers)
          (scan-to-strings '(:sequence link-label) " [aa]: http://foo.bar")))
    (ensure-same (aref registers 0) "aa")
    (ensure-same (aref registers 1) "http://foo.bar")
    (ensure-same (aref registers 2) nil)))

(addtest (test-link-label)
  test-link-with-title
  (bind (((values nil registers)
          (scan-to-strings '(:sequence link-label) 
                           "  [aa]: http://foo.bar \"best foos\"")))
    (ensure-same (aref registers 0) "aa")
    (ensure-same (aref registers 1) "http://foo.bar")
    (ensure-same (aref registers 2) "best foos")))

;;; ---------------------------------------------------------------------------

(deftestsuite test-inline-links (test-regexes) ())
(addtest (test-inline-links)
  test-1
  (ensure-same
   (nth-value 1
              (scan-to-strings 
               '(:sequence inline-link)
               "This is an [in-line](http://www.google.com/ \"Link to Google\") link"))
   #("in-line" "http://www.google.com/" "Link to Google")
   :test 'equalp))

(addtest (test-inline-links)
  test-2
  (ensure-same
   (nth-value 1
              (scan-to-strings 
               '(:sequence inline-link)
               "This is an [in-line](http://www.google.com/) link with no title"))
   #("in-line" "http://www.google.com/" nil)
   :test 'equalp))

(addtest (test-inline-links)
  test-2
  (ensure-same
   (scan-to-strings 
    '(:sequence inline-link)
    "This is not an (in-line)(http://www.google.com/) link with no title") nil))

;;; ---------------------------------------------------------------------------

(deftestsuite test-reference-links (test-regexes) ())

(addtest (test-reference-links)
  test-1
  (ensure-same
   (nth-value 1
              (scan-to-strings 
               '(:sequence reference-link)
               "This is an [in-line][id] link"))
   #("in-line" "id")
   :test 'equalp))

(addtest (test-reference-links)
  test-2
  (ensure-same
   (nth-value 1
              (scan-to-strings 
               '(:sequence reference-link)
               "This is an [in-line] [id] link with no title"))
   #("in-line" "id")
   :test 'equalp))

