(in-package cl-markdown)

;;?? stub
(defun strip (text)
  "remove leading and trialing whitespace" 
  text)

#| re
see http://www.amk.ca/python/howto/regex/

group() returns the substring that was matched by the RE.

Group 0 is always present; it's the whole RE, so MatchObject methods all have group 0 as their default argument. Later we'll see how to express groups that don't capture the span of text that they match.

DOTALL, S
Make . match any  character, including newlines

IGNORECASE, I
Do case-insensitive matches

LOCALE, L
Do a locale-aware match

MULTILINE, M
Multi-line matching,  affecting ^ and $

VERBOSE, X
Enable verbose REs,  which can be organized more cleanly and understandably.
|#

#| __str__

Converting an object to a string, as with the print statement or with the str() conversion function, can be overridden by overriding __str__. Usually, __str__ returns a formatted version of the objects content. This will NOT usually be something that can be executed.
|#


;;; was in auxiliary classes
(defun print_error(string)
  "Print an error string to stderr."
  (warn string))
    
;;; ---------------------------------------------------------------------------

;;; was in auxiliary classes
(defun dequote (string)
  "Removes (one set of) quotes from around a string."
  (if (or (and (string-starts-with string "\"")
               (string-ends-with string "\""))
          (and (string-starts-with string "'")
               (string-ends-with string "'")))
    (values (subseq string 1 (- (size string) 1)))
    (values string)))