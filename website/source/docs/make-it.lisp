(in-package #:cl-markdown)

(defun md (system name)
  (let ((input (dsc:system-relative-pathname 
		system
		(format nil "website/source/~a.md" name)))
	(output (system-relative-pathname 
		 system
		 (format nil "website/output/~a.html" name))))
    (ensure-directories-exist output)
    (markdown input :stream output :format :html
	      :additional-extensions '(docs today now footnote
				       footnotes table-of-contents))))

(md 'cl-markdown "docs/user-guide")

(defun md-all ()
  (dolist (file
	    (directory (dsc:system-relative-pathname 
			'asdf-install 
			(make-pathname 
			 :directory '(:relative :back "website" "source" 
				      "tutorial")
			 :name :wild
			 :type "md"))))
    (when (probe-file file)
      (print file)
      (md (pathname-name file)))))

