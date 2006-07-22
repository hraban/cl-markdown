(in-package #:cl-markdown-test)

;;; from ASDF-Install

#-:digitool
(defun system-namestring (pathname)
  (namestring (truename pathname)))

#+:digitool
(defvar *start-up-volume*
  (second (pathname-directory (truename "ccl:"))))

#+:digitool
(defun system-namestring (pathname)
  ;; this tries to adjust the root directory to eliminate the spurious
  ;; volume name for the boot file system; it also avoids use of
  ;; TRUENAME as some applications are for not yet existent files
  (let ((truename (probe-file pathname)))
    (unless truename
      (setf truename
            (translate-logical-pathname
             (merge-pathnames pathname *default-pathname-defaults*))))
    (let ((directory (pathname-directory truename)))
      (flet ((string-or-nil (value) (when (stringp value) value))
             (absolute-p (directory) (eq (first directory) :absolute))
             (root-volume-p (directory)
               (equal *start-up-volume* (second directory))))
        (format nil "~:[~;/~]~{~a/~}~@[~a~]~@[.~a~]"
                (absolute-p directory)
                (if (root-volume-p directory) (cddr directory) (cdr directory))
                (string-or-nil (pathname-name truename))
                (string-or-nil (pathname-type truename)))))))

