(in-package #:cl-markdown-test)

(deftestsuite test-definition-list (test-snippets)
  ())

#|
Term one
: Definition one
    * More stuff in def one
    * (a list!)

Term two

: Definition one for term two
: Definition two for term two

Term three
Another term three
: definition for term three

(markdown "
My things

Green
 : beautiful
")

(markdown "
My things

Green
 : beautiful
 : toasty
")

(markdown "
My things

Red
Green
 : beautiful
 : toasty
")

(markdown "
My things

Green
 : beautiful

 : toasty
")

(markdown "
My things

Green
 : beautiful
    * one
    * two
 : toasty
")

(markdown "
My things

Green
: beautiful

one

two

three

 : toasty
")


(markdown "Term 1

:   This is a definition with two paragraphs. Lorem ipsum
    dolor sit amet, consectetuer adipiscing elit. Aliquam
    hendrerit mi posuere lectus.

    Vestibulum enim wisi, viverra nec, fringilla in, laoreet
    vitae, risus.

:   Second definition for term 1, also wrapped in a paragraph
    because of the blank line preceding it.

Term 2

:   This definition has a code block, a blockquote and a list.

        code block.

    > block quote
    > on two lines.

    1.  first list item
    2.  second list item"

)

|#

(addtest (test-definition-list)
  one-term-two-descriptions
  (check-html-output
"Punt
 : Kick a ball
 : Take a bet"
"<dl>
<dt>Punt</dd>
<dd>Kick a ball</dd>
<dd>Take a bet</dd>
</dl>"))

(addtest (test-definition-list)
  two-terms-one-description
  (check-html-output
"Punt
Dance
: Take a bet"
"<dl>
<dt>Punt</dt>
<dt>Dance</dt>
<dd>Take a bet</dd>
</dl>"))

(addtest (test-definition-list)
  two-simple-entries
  (check-html-output
"Punt
 : Take a bet

Dance
: Shake a jig"
"<dl>
<dt>Punt</dt>
<dd>Take a bet</dd>
<dt>Dance</dt>
<dd>Shake a jig</dd>
</dl>"))

#+(or)
(addtest (test-definition-list)
  complex
  (check-html-output
 "Term 1

:   This is a definition with two paragraphs. Lorem ipsum
    dolor sit amet, consectetuer adipiscing elit. Aliquam
    hendrerit mi posuere lectus.

    Vestibulum enim wisi, viverra nec, fringilla in, laoreet
    vitae, risus.

:   Second definition for term 1, also wrapped in a paragraph
    because of the blank line preceding it.

More stuff

Term 2

:   This definition has a code block, a blockquote and a list.

        code block.

    > block quote
    > on two lines.

    1.  first list item
    2.  second list item"
"
"))

(addtest (test-definition-list)
  mulit-line-definition
  (check-html-output
"Punt
 : Take a bet

    that is right."
"<dl>
<dt>Punt</dt>
<dd>Take a bet

<p>that is right.</p></dd>
</dl>"))
