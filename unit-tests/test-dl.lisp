(in-package #:cl-markdown-test)

(deftestsuite test-definition-list (test-snippets)
  ())

#|
: Definition one
    * More stuff in def one
    * (a list!)
: Definition two
    * foo
|#

(addtest (test-definition-list)
  one-term-two-descriptions
  (check-html-output
": Punt
    * Kick a ball
    * Take a bet"
"<dl>
<dt>Punt</dd>
<dd>Kick a ball</dd>
<dd>Take a bet</dd>
</dl>"))

(addtest (test-definition-list)
  two-terms-one-descriptions
  (check-html-output
": Punt
: Dance
    * Take a bet"
"<dl>
<dt>Punt</dt>
<dt>Dance</dt>
<dd>Take a bet</dd>
</dl>"))

(addtest (test-definition-list)
  two-entries
  (check-html-output
": Punt
    * Take a bet
: Dance
    * Shake a jig"
"<dl>
<dt>Punt</dt>
<dd>Take a bet</dd>
<dt>Dance</dt>
<dd>Shake a jig</dd>
</dl>"))

