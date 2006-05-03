(in-package cl-markdown-test)

(defun html->lml (source)
  (html-parse:parse-html source))

;;; ---------------------------------------------------------------------------

(defun compare-results (one two)
  (string-equal one two))

;;; ---------------------------------------------------------------------------

(defun run-tests ()
  (fad:walk-directory
   (merge-pathnames 
    (make-pathname :name nil :type nil :directory '(:relative "test")) 
    (system-source-file 'cl-markdown)))  
    
   (lambda (file)
     (compare-results (markdown file)
                      (html->lml (make-pathname :type "html"
                                                :defaults file))))
   :test (lambda (file)
           (string-equal (pathname-type file) "text"))))