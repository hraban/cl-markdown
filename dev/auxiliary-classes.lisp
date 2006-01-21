(in-package cl-markdown)

#|
# ======================================================================
# ========================== MISC AUXILIARY CLASSES ====================
# ======================================================================
|#

(defclass* html-stash () 
  "This class is used for stashing HTML objects that we extract in the 
beginning and replace with place-holders."
  ((html-counter 0 a)
   (raw-html-blocks (make-container 'list-container) r)))

;;; ---------------------------------------------------------------------------

(defmethod store ((self html-stash) html)
  " Saves an HTML segment for later reinsertion.  Returns a
           placeholder string that needs to be inserted into the
           document. "
  (append-item (raw-html-blocks self) html)

  placeholder = HTML_PLACEHOLDER % self.html_counter
  (incf (html-counter self))
  (values placeholder))

;;; ---------------------------------------------------------------------------

(defclass* block-guru ()
  ())

;;; ---------------------------------------------------------------------------
  
(defmethod _findHead ((self block-guru) lines fn &optional (allowBlank nil))

  "Functional magic to help determine boundaries of indented blocks.

           @param lines: an array of strings
           @param fn: a function that returns a substring of a string
                      if the string matches the necessary criteria
           @param allowBlank: specifies whether it's ok to have blank
                      lines between matching functions
           @returns: a list of post processes items and the unused
                      remainder of the original list "

  (let ((items (make-container 'list-container))
        (item -1)
        (i 0))
    (loop for line in lines do
          (when (and (not (strip line)) (not allow-blank))
            (return (values items (subseq lines i))))
          
          (cond ((and (not (strip line)) allow-blank)
                 ; If we see a blank line, this _might_ be the end
                 (incf i)
                 
                 ; find the next non-blank line
                 (loop for j from i to (size lines) do
                       (when (strip (nth-element lines j))
                         (setf next (nth-element lines j))
                         (return)))
                 
                 ; There is no more text => this is the end
                 (return)
                 
                 ; Check if the next non-blank line is still a part of the list
                 (setf part (funcall fn next))
                 (if part
                   (append-item items "")
                   (return))
                 
                 (setf part (funcall fn line))
                 (cond (part 
                        (append-item items part)
                        (incf i)
                        continue
                        )
                       (t
                        (return (values items (subseq lines i))))))
                (t 
                 (incf i))))

    (values items (subseq lines i))))

;;; ---------------------------------------------------------------------------

(defmethod detabbed_fn ((self block-guru) line)
  " An auxiliary method to be passed to _findHead "
  m = RE.regExp['tabbed'].match(line)
  if m:
  return m.group(4)
  else :
  return None
  )

;;; ---------------------------------------------------------------------------

(defmethod detectTabbed ((self block-guru) lines)

  return self._findHead (lines, self.detabbed_fn, allowBlank = 1)


