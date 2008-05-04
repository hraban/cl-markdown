(in-package #:common-lisp-user)

(defpackage #:cl-markdown-test
  (:use #:common-lisp #:lift #:metatilities #:cl-containers
        #:cl-ppcre #:cl-markdown #:trivial-shell)
  (:shadowing-import-from #:lift
                          #:with-timeout)
  (:shadowing-import-from #:metatilities
                          #:copy-file)
  (:import-from #:trivial-shell
		#:shell-command)
  (:import-from #:cl-markdown
		#:scan-lines-with-scanners
                #:atx-header-markup-class
                #:blockquote-stripper
                #:chunk-source
                #:line-could-be-link-reference-title-p
                #:line-indentation
                #:line-is-blockquote-p
                #:line-is-code-p
                #:line-is-empty-p
                #:line-is-horizontal-rule-p
                #:line-starts-with-bullet-p
                #:line-starts-with-number-p
		#:it-starts-with-block-level-html-p
                #:markdown
                #:one-tab-stripper
                #:remove-atx-header
                #:remove-bullet
                #:remove-number
                #:strippers
                
                #:handle-setext-headers
                #:lines
                #:indentation
                #:maybe-strip-line
                #:chunks
                #:markup-class 
                #:paragraph?
                #:link-info
                #:id
                #:properties
		#:title
                
                #:header1
                #:header2
                #:header3
                #:header4
                #:header5
                #:header6

                #:*spaces-per-tab*
                #:*parsing-environment*))