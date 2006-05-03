(in-package cl-markdown-test)

(defparameter *test-source-directory* 
  (system-relative-path
   'cl-markdown 
   (make-pathname :directory ";unit-tests;markdown-tests;")))

(defparameter *test-output-directory*
  (system-relative-path
   'cl-markdown 
   (make-pathname :directory ";website;output;comparison-tests;")))

(defun compare-markdown-and-cl-markdown (basename)
  (cl-markdown-and-tidy basename)
  (markdown-and-tidy basename)
  (create-comparison-file basename))   

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
         (iterate-elements 
          (directory 
           (make-pathname :name :wild :type "text" :defaults *test-source-directory*))
          (lambda (file)
            (let* ((entry-file (comparison-file-name (pathname-name file)))
                   (entry (namestring (make-pathname :name (pathname-name entry-file)
                                                     :type "html"))))
              (lml2:html
               ((:span :class "index-entry") 
                ((:a :href entry) (lml2:lml-princ entry)))))))))))))

(defun compare-all ()
  (iterate-elements 
   (directory (make-pathname :name :wild :type "text" :defaults *test-source-directory*))
   (lambda (file)
     (handler-case
       (compare-markdown-and-cl-markdown (pathname-name file))
       (error (c) 
              (create-error-file (pathname-name file) c)))))
  (create-main-comparison-page))

(defun cl-markdown-and-tidy (basename)
  (let* ((inpath (make-pathname :type "text"
                                :name basename 
                                :defaults *test-source-directory*))
         (output (make-pathname :type "html"
                                :name basename
                                :defaults *test-source-directory*)))
    (cl-markdown::render-to-stream (markdown inpath) :html output)
    (tidy basename "html" "tidy")
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
         (:P "Error during parsing of '" (lml2:lml-princ basename) ".")
         (:P 
          (:pre
           (lml2:lml-princ
            (html-encode:encode-for-pre 
             (html-encode:encode-for-http
              (format nil "~A" condition))))))))))))

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
    tidy-output))

(defun comparison-file-name (basename)
  (make-pathname :defaults *test-output-directory*
                 :type "html"
                 :name (concatenate 'string basename "-compare")))

(defun create-comparison-file (basename)
  (let ((cl-file (make-pathname :type "tidy"
                                :name basename 
                                :defaults *test-source-directory*))
        (md-file (make-pathname :type "down"
                                :name basename 
                                :defaults *test-source-directory*))
        (output (comparison-file-name basename)))
    (ensure-directories-exist output)
    (with-new-file (s output)
      (lml2:html-stream 
       s
       (lml2:html
        (:head (:title "CL-Markdown / Markdown Comparison")
               ((:link :rel "stylesheet" :href "style.css")))
        (:body
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
            (lml2:lml-princ
             (html-diff::html-diff (file->string md-file) (file->string cl-file)))))
          
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
           (:h1 "HTML from CL Markdown")
           ((:div :class "section-contents")
            (:pre
             (lml2:lml-princ
              (html-encode:encode-for-pre 
               (html-encode:encode-for-http
                (file->string (make-pathname :type "text"
                                             :name basename 
                                             :defaults *test-source-directory*)))))))))))))))

(defun file->string (pathname)
  (apply 'concatenate 
         'string
         (collect-elements
          (make-iterator (make-pathname :defaults pathname) 
                         :treat-contents-as :lines 
                         :skip-empty-chunks? nil))))

#|


(render-to-stream 
 (markdown #P"Billy-Pilgrim:Users:gwking:darcs:cl-markdown:unit-tests:markdown-tests:Ordered and unordered lists.text")
 :lml2 :none)

(render-to-stream 
 (markdown #P"Billy-Pilgrim:Users:gwking:darcs:cl-markdown:unit-tests:markdown-tests:Nested blockquotes.text")
 :lml2 :none)

Nested blockquotes
(markdown-tidy "Horizontal rules")
(markdown-tidy "Ordered and unordered lists")

(markdown-tidy "bullets-and-numbers-1")

(markdown-and-tidy "bullets-and-numbers-1")
(cl-markdown-and-tidy "bullets-and-numbers-1")

(metashell:shell-command "/usr/bin/tidy --show-body-only 1 --quiet 1 --show-warnings 0 /Users/gwking/darcs/cl-markdown/unit-tests/markdown-tests/bullets-and-numbers-1.html > /Users/gwking/darcs/cl-markdown/unit-tests/markdown-tests/bullets-and-numbers-1.tidy")


|#