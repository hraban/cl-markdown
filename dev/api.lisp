(in-package #:cl-markdown)


(defgeneric reset (thing)
  )


(defgeneric (setf document-property) (value name))

(defgeneric render-to-stream (document style stream-specifier)
  )

(defgeneric main-parent (document)
  )

(defgeneric handle-spans (document)
  )

(defgeneric scan-one-span (what scanner-name scanner scanners)
  )

(defgeneric process-span-in-span-p (sub-span current-span)
  )

(defgeneric unconvert-escapes (what)
  )

(defgeneric render (document style stream)
  )

(defgeneric it-starts-with-block-level-html-p (chunk)
  )

(defgeneric markup-class-mergable-p (what)
  )

(defgeneric merge-lines-in-chunks (what)
  )

(defgeneric can-merge-lines-p (first second)
  )

(defgeneric handle-paragraph-eval-interactions (what)
  )

(defgeneric encode-html (what encoding-method &rest codes)
  )

(defgeneric markup-class-for-html (what)
  )

(defgeneric render-span-to-html (kind body encoding-method)
  )

(defgeneric generate-link-output (link-info text)
  )

(defgeneric add-html-header-p (document)
  )

(defgeneric render-plain (what)
  )

(defgeneric render-span-plain (kind body)
  )

(defgeneric process-span-for (kind command args)
  )

(defgeneric generate-link-output-for-kind (kind link-info text)
  )

(defgeneric process-span (name registers)
  (:documentation "Called during span processing on each match of name in the
document. Registers a list of the registers captured by names regular expression.
Returns a possibly new set of registers.")
  (:method ((name t) (registers t))
           (values registers)))

(defgeneric print-html-markup (markup stream)
  )
