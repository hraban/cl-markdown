(in-package #:cl-markdown-test)

#|
source .text
cl-markdown .html
  tidy      .xxxx
markdown    .down
  tidy      .mark
|#

#+(or)
(compare-all)

#+(or)
(compare-markdown-and-cl-markdown
 (pathname-name 
  (first (directory 
	  (make-pathname :name :wild 
			 :type "text" 
			 :defaults *test-source-directory*)))))

#+(or)
(compare-markdown-and-cl-markdown "Auto Links")

(defvar *errors* nil)
(defvar *all-wells* nil)
(defvar *data* nil
  "What a hack! Shoot me")

(defparameter *test-source-directory* 
  (system-relative-pathname
   'cl-markdown 
   (make-pathname :directory '(:relative "unit-tests" "markdown-tests"))))

(defparameter *test-output-directory*
  (system-relative-pathname
   'cl-markdown 
   (make-pathname :directory 
		  '(:relative "website" "output" "comparison-tests"))))

(defun compare-markdown-and-cl-markdown (basename)
  (cl-markdown-and-tidy basename)
  (markdown-and-tidy basename)
  (create-comparison-file basename))   

(defun compare-all ()
  (setf *errors* nil
        *all-wells* nil)
  (iterate-elements 
   (directory (make-pathname :name :wild :type "text" :defaults *test-source-directory*))
   (lambda (file)
     (handler-case
       (compare-markdown-and-cl-markdown (pathname-name file))
       (error (c) 
              (push (pathname-name file) *errors*)
              (create-error-file (pathname-name file) c)))))
  (create-main-comparison-page)
  (copy-file (make-pathname :type "css"
                            :name "style" 
                            :defaults *test-source-directory*)
             (make-pathname :type "css"
                            :name "style"
                            :defaults *test-output-directory*)
             :if-exists :supersede))

(defun create-main-comparison-page ()
  (let ((output (make-pathname :type "html"
                               :name "index"
                               :defaults *test-output-directory*)))
    (ensure-directories-exist output)
    (with-new-file (s output)
      (lml2:html-stream 
       s
       (lml2:html
        (:head (:title "Index | CL-Markdown / Markdown Comparison")
               ((:link :rel "stylesheet" :href "style.css")))
        (:body
         ((:div :id "contents")
          (:P 
           "Below are the results of running "
           ((:A :href "http://www.common-lisp.net/project/cl-markdown") "CL-Markdown")
           " and the Perl " ((:a :href "http://www.daringfireball.net/markdown") "Markdown") 
           " script on the same input. You'll see that the current version of CL-Markdown performs well on most documents and poorly on a few. You'll also find that the rendered HTML can be very similar even where the diffs between outputs contains many insertions and deletions.")
          (:P 
           "This will be updated regularly. The most recent update was "
           (lml2:lml-princ (format-date "%e %B %Y" (get-universal-time))))

          (:H2 "Comparison Tests")
          
          (iterate-elements 
           (directory 
            (make-pathname :name :wild :type "text" :defaults *test-source-directory*))
           (lambda (file)
             (bind ((entry-file (comparison-file-name (pathname-name file)))
                    (entry (namestring (make-pathname :name (pathname-name entry-file)
                                                      :type "html")))
                    (data (find (pathname-name file) *data*
                                :test #'string-equal :key #'car))
                    ((nil replace insert delete) (or data (list nil nil nil nil))))
               (lml2:html
                ((:span :class 
                        (cond ((find (pathname-name file) *errors* :test #'string-equal)
                               "index-entry error")
                              ((find (pathname-name file) *all-wells* :test #'string-equal)
                               "index-entry good")
                              (t "index-entry")))
                 ((:a :href entry) (lml2:lml-princ entry)
                    (unless (and (and replace (zerop replace))
                                 (and delete (zerop delete))
                                 (and insert (zerop insert)))
                      (lml2:lml-format " (~D, ~D, ~D)" replace delete insert))))))))
          
          ((:div :id "notes") 
           (:P "In the rare case that CL-Markdown produces invalid HTML. Most browsers will still display the output but "
              ((:A :href "tidy") "Tidy") " reports errors and produces no output. This will show up as a blank section on the comparison page. As far as I know, the HTML CL-Markdown is now always valid.")
           (:P "Files with this " ((:span :class "error") "color") " had Lisp errors during the run. "
               "Files with this " ((:span :class "good") "color") " had no differences from Markdown output during the run."
               "The numbers in parentheses represent the number of replacements, inserts, and deletes that occurred during the diff."))
          
          ((:div :id "footer") "end 'o page"))))))))

(defun cl-markdown-and-tidy (basename)
  (let* ((inpath (make-pathname :type "text"
                                :name basename 
                                :defaults *test-source-directory*))
         (output (make-pathname :type "html"
                                :name basename
                                :defaults *test-source-directory*)))
    (markdown inpath :format :html :stream output)
    (tidy basename "html" "xxxx")
    output))

(defun create-error-file (basename condition)
  (let ((output (comparison-file-name basename)))
    (ensure-directories-exist output)
    (with-new-file (s output)
      (lml2:html-stream 
       s
       (lml2:html
        (:head (:title "CL-Markdown / Markdown Comparison")
               ((:link :rel "stylesheet" :href "style.css")))
        (:body
         ((:div :id "contents")
          (:P "Error during parsing of '" (lml2:lml-princ basename) "'.")
          ((:a :href "index.html") "Back to index")
          (:P 
           (:pre
            (lml2:lml-princ
             (html-encode:encode-for-pre 
              (html-encode:encode-for-http
               (format nil "~A" condition))))))
          
          (:div
           ((:div :id "original-source")
            (:h1 "Original source")
            ((:div :class "section-contents")
             (:pre
              (lml2:lml-princ
               (html-encode:encode-for-pre 
                (file->string (make-pathname 
                               :type "text"
                               :name basename 
                               :defaults *test-source-directory*))))))))
          ((:div :id "footer") "end 'o page"))))))))

(defun markdown-and-tidy (basename)
  (let* ((inpath (make-pathname :type "text"
                                :name basename 
                                :defaults *test-source-directory*))
         (outpath (make-pathname :type "mark"
                                 :name basename 
                                 :defaults *test-source-directory*)))
    (metashell:shell-command 
     (format nil "/usr/local/bin/markdown '~a' > '~A'"
             (system-namestring inpath) (system-namestring outpath)))
    
    (tidy basename "mark" "down")
    outpath))

(defun tidy (basename input-type output-type)
  (let* ((inpath (make-pathname :type input-type
                                :name basename 
                                :defaults *test-source-directory*))
         (tidy-output (make-pathname :type output-type
                                     :name basename 
                                     :defaults *test-source-directory*))
         (command (format nil 
                          "/usr/bin/tidy --show-body-only 1 --quiet 1 --show-warnings 0 '~A' > '~A'"
                          (system-namestring inpath)
                          (system-namestring tidy-output))))
    (metashell:shell-command command)
    (when (zerop (kl:file-size tidy-output))
      ;; an error in the HTML
      (warn "HTML Error for ~A" basename))
    tidy-output))

(defun comparison-file-name (basename)
  (make-pathname :defaults *test-output-directory*
                 :type "html"
                 :name (concatenate 'string basename "-compare")))

(defun create-comparison-file (basename)
  (bind ((cl-file (make-pathname :type "xxxx"
                                 :name basename 
                                 :defaults *test-source-directory*))
         (md-file (make-pathname :type "down"
                                 :name basename 
                                 :defaults *test-source-directory*))
         ((values diff replace insert delete)
          (html-diff::html-diff (file->string md-file) (file->string cl-file)))
         (output (comparison-file-name basename)))
    (push (list basename replace insert delete) *data*)
    (ensure-directories-exist output)
    (with-new-file (s output)
      (lml2:html-stream 
       s
       (lml2:html
        (:head (:title "CL-Markdown / Markdown Comparison")
               ((:link :rel "stylesheet" :href "style.css")))
        (:body
         ((:div :id "contents")
          ((:div :id "header")
           (:h1 "File: " (lml2:lml-princ basename) ".text"))
          ((:a :href "index.html") "Back to index")
          (:div
           ((:div :id "cl-markdown-output")
            (:h1 "CL-Markdown")
            ((:div :class "section-contents")
             (lml2:insert-file cl-file)))
           ((:div :id "markdown-output")
            (:h1 "Markdown")
            ((:div :class "section-contents")
             (lml2:insert-file md-file))))
          (:div
           ((:div :id "diff-output")
            (:h1 "HTML Difference")
            ((:div :class "section-contents")
             (cond ((and (zerop insert) (zerop delete) (zerop replace))
                    (push basename *all-wells*)
                    (lml2:lml-princ "No differences"))
                   (t
                    (lml2:html
                     (:P 
                      "Insert: " (lml2:lml-princ insert)
                      ", Delete: " (lml2:lml-princ delete)
                      ", Replace " (lml2:lml-princ replace))
                     (lml2:lml-princ
                      diff))))))
           ((:div :id "cl-markdown-html")
            (:h1 "HTML from CL Markdown")
            ((:div :class "section-contents")
             (:pre
              (lml2:lml-princ
               (html-encode:encode-for-pre 
                (html-encode:encode-for-http
                 (file->string cl-file))))))))
          (:div
           ((:div :id "original-source")
            (:h1 "Original source")
            ((:div :class "section-contents")
             (:pre
              (lml2:lml-princ
               (html-encode:encode-for-pre 
                (html-encode:encode-for-http
                 (file->string (make-pathname :type "text"
                                              :name basename 
                                              :defaults *test-source-directory*)))))))))
          ((:div :id "footer") "end 'o page"))))))))

(defun file->string (pathname)
  (apply 'concatenate 
         'string
         (with-iterator (iterator (make-pathname :defaults pathname) 
                                  :treat-contents-as :lines 
                                  :skip-empty-chunks? nil) 
           (collect-elements
            iterator
            :transform (lambda (line) 
                         (format nil "~%~A" line))))))

