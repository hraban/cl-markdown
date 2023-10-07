(defsystem cl-markdown-comparisons
  :version "0.1"
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Hraban Luyat <hraban@0brg.net>"
  :licence "GPL-3.0-only"
  :components ((:module "unit-tests"
                        :components ((:file "package")
                                     (:file "framework"
                                            :depends-on ("package"))
                                     (:file "comparison"
                                            :depends-on ("framework")))))
  :depends-on ( :cl-markdown
                :lml2
		:cl-html-diff
                :html-encode
                :trivial-shell
		;; probably not needed if we rearranged more...
		:lift))
