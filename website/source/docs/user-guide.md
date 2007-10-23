{set-property html yes}
{set-property title "CL-Markdown User's Guide"}
{set-property style-sheet user-guide}

# CL-Markdown - Quick Start

{table-of-contents :start 2 :depth 3}

CL-Markdown is an enhanced version of John Gruber's [Markdown][] text 
markup langauge. Markdown's goal is to keep text readable as *text* and 
as HTML. CL-Markdown keeps this principle and adds a flexible extension 
mechanism so that you can build complex documents easily.

 [Markdown]: http://daringfireball.net/projects/markdown/
 

### Getting Starting

The easiest way to install CL-Markdown is using the [bundle][]. You can 
also use [ASDF-Install][], download tarballs or grab the sources directly
(usings [darcs][]). If you do use the bundle, here is what you'd do:

    shell> cd <lisp-sources>
    shell> curl http://common-lisp.net/project/cl-markdown/cl-markdown-bundle.tar.gz > cl-markdown-bundle.tar.gz
    shell> tar -zxvf cl-markdown-bundle.tar.gz
    shell> lisp
    ;; Super Lisp 5.3 (just kidding)
    lisp: (require 'asdf)
    lisp: (load "cl-markdown-bundle/cl-markdown.asd")
    lisp: (asdf:oos 'asdf:load-op 'cl-markdown)
    lisp: (in-package cl-markdown)

The top-level CL-Markdown command is `markdown`. It creates a `document`
from a source (pathname, stream or string) and then sends the 
document to a stream in a `format`. The default format is `:html` and the 
default output is `t` (which sends the output to `*standard-output*`.). You 
can use an already open stream for output, provide a pathname to a file (which
will be overwritten!) or use the symbol `nil` to direct output to a new stream.
At this time, support for formats other than HTML is not provided.
For example:

    lisp: (markdown "# Hello *there*")
    "<h1>Hello <em>there</em></h1>"

CL-Markdown implements most of John Gruber's [specification][markdown-specification] (e-mails and some edges cases). It also adds a new syntax for extensions.

 [markdown-specification]: 

### Function calls: \{ and \}

Calling extension functions requires three things:

1. writing (or finding) the extension that you want
2. telling CL-Markdown that you want to use the extension
3. writing your Markdown text with calls to the extension

The last part is the easiest; all you need to do is open a curly
brace, type the name of extension function, type in the arguments
(separated by spaces) and type a closing curly brace. For example:

"\{now\}" will generate the text "{now}".

The second step is necessary because CL-Markdown won't recognize
functions as functions unless you tell it to up front. After all, you
wouldn't want to allow people to execute arbitrary code; it **might**
be a security risk (smile). Because CL-Markdown operates in two stages, there
are two times when functions can be called: during parsing and during
rendering. Functions active during these stages are keep in the
special variables `*render-active-functions*` and
`*parse-active-functions*`. 

An example maight make this clearer. First, we'll call Markdown
without specifying any functions:

    ? (markdown "Today is {today}. It is {now}." 
      :format :html :stream t)
    <P>Today is 
    ; Warning: Inactive or undefined CL-Markdown function TODAY
    ; While executing: #<STANDARD-METHOD RENDER-SPAN-TO-HTML ((EQL EVAL) T)>
    . It is 
    ; Warning: Inactive or undefined CL-Markdown function NOW
    ; While executing: #<STANDARD-METHOD RENDER-SPAN-TO-HTML ((EQL EVAL) T)>
    . </P>
    
As you can see, the functions weren't ones that CL-Markdown was ready
to recognize, so we got warnings and no text was generated. If we
tell CL-Markdown that `today` and `now` should be treated as
functions, then we see a far prettier picture:

    ? (let ((*render-active-functions* 
             (append '(today now) *render-active-functions*)))
        (markdown "Today is {today}. It is {now}." 
            :format :html :stream t))
    <P>Today is 1 August 2006. It is 11:36. </P>

By now, we've seen how to include function calls in CL-Markdown
documents and how to generate those documents with CL-Markdown. The
final piece of the puzzle is actually writing the extensions.


#### Writing Cl-Markdown extensions

There are several ways to write extensions. {footnote Extensions beg for
a little {abbrev DSL Domain Specific Language} but those macros are 
still to be written.} The easiest is
one is to write functions active during rendering that return the text
that you wish to be included in the document. For example:

    (defun today (phase arguments result)
      (declare (ignore phase arguments result))
      (format-date "%e %B %Y" (get-universal-time)))

The format-date command is part of [metatilities][]; it returns a string
of the date using the C library inspired date format. This string is
placed in the document whereever the function call (\{today\}) is
found.
 
 [metatilities]: 

Alternately, one can use the `*output-stream*` variable to insert more
complicated text. This would look like:

    (defun now (phase arguments result)
      (declare (ignore phase arguments result))
      (format *output-stream* "~a" 
        (format-date "%H:%M" (get-universal-time)))
      nil)

(Note that `now` returns `nil` so that the date isn't inserted
twice!).

The other alternative is to use your function calls to alter the
structure of the CL-Markdown document and then let Markdown deal with
some or all of the rest. The `anchor` extension provides an example of
this:

    (defun anchor (phase &rest args)
      (ecase phase
        (:parse
         (let ((name (caar args))
               (title (cadar args)))
           (setf (item-at (link-info *current-document*) name)
                 (make-instance 'link-info
                   :id name :url (format nil "#~a" name) 
                   :title (or title "")))))
        (:render (let ((name (caar args)))
                   (format nil "<a id='~a' id='~a'></a>"
                           name name)))))

`Anchor` makes it easier to insert anchors into your document and to
link to those anchors from elsewhere. It is active during both parsing
and rendering. During the parsing phase, it uses it's arguments to
determine the name and title of the link and places this into the
current document's link information table. During rendering, it
outputs the HTML needed to mark the link. {footnote If you would like 
to see more examples, look in the files `extensions.lisp`
or `footnotes.lisp`.}
 
<hr>

{footnotes}
