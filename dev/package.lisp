(in-package #:common-lisp-user)

(defpackage #:cl-markdown
  (:use #:common-lisp #:metatilities #:cl-containers #:cl-ppcre)
  (:export
   #:handle-spans
   #:markdown
   #:markdown-many
   #:render-to-stream
   #:*current-document*
   #:*output-stream*
   #:document-property)
  (:nicknames #:markdown)
  (:export 
   #:*render-active-functions*
   #:*parse-active-functions*
   #:anchor
   #:table-of-contents
   #:property
   #:set-property
   #:render
   #:render-documentation)
  ;; handy (?) regular expressions
  (:export
   #:emphasis-1 #:emphasis-2
   #:strong-1 #:strong-2
   #:backtick
   #:auto-link #:auto-mail
   #:html #:entity
   #:hostname-char #:hostname
   #:pathname-char #:url-pathname
   #:url #:url-no-registers
   #:bracketed #:link+title
   #:reference-link #:inline-link #:link-label)
  (:export
   #:footnote
   #:footnotes
   #:find-documentation
   #:add-documentation-strategy))

(defpackage #:cl-markdown-user
  (:use #:common-lisp #:metatilities #:cl-markdown)
  (:import-from #:cl-markdown
   #:footnote
   #:footnotes))
