{include resources/header.md}
{set-property title "CL-Markdown - Markdown and More"}

<div class="contents">
<div class="system-links">

  * [Mailing Lists][3]
  * [Getting it][4]
{remark  * [Documentation][5]}
  * [News][6]
{remark  * [Test results][tr]}
  * [Changelog][7]

   [3]: #mailing-lists
   [4]: #downloads
   [5]: documentation/ "documentation link"
   [6]: #news
   [7]: changelog.html
   [tr]: test-report.html

</div>
<div class="system-description">

### What it is

(Note: CL-Markdown just split off it's Lisp documentation
abilities into the [docudown][] project. Don't be alarmed.
Everything is good.)

[Markdown][] is [John Gruber][df]'s text markup langauge and
the Perl program that converts documents written in that
language into HTML. CL-Markdown is a Common Lisp rewrite of
Markdown. CL-Markdown is licensed under the [MIT
license][mit-license].

You can see the source of this page by clicking in the
address bar of your browser and changing the extension from
`html` to `text`. For example, this page's source is at
[index.text](index.text).

You can view a comparison of Markdown and CL-Markdown output
[here][8].

{anchor mailing-lists}

### Mailing Lists

  * [devel-list][]: A list for questions, patches, bug
    reports, and so on; It's for everything other than
    announcements.

{anchor downloads}

### Where is it

metabang.com is switching from [darcs][] to [git][]
for source control; the current cl-markdown repository is on
[github][github-cl-markdown] and you can clone it using:

    git clone git://github.com/gwkkwg/cl-markdown

(note that this won't let you build CL-Markdown unless you
also get all of its dependencies which I should list but don't
because I haven't found (er, made) the time to automate the
process yet...)

The easiest way to get setup with CL-Markdown is by using
[QuickLisp][] or [ASDF-Install][14] (deprecated). If that
doesn't float your boat, there is a handy [gzipped tar
file][15]

{anchor news}

### What is happening

<dl>
    <dt>8 January 2011</dt>
    <dd>Moved to github. Very minor cleanup.</dd>
    <dt>28 May 2008</dt>
    <dd>Many small improvements, bug fixes, tweaks, and
        extensions. The biggest change, however, is that I've
        move the Lisp documentation work into it's own
        [project][docudown]. This keeps CL-Markdown simpler.
        The dependencies on [moptilities][] and
        [defsystem-compatibility][] have both been removed.
        A dependency on [anaphora][clnet-anaphora] has been added.
    </dd>
    <dt>30 August 2007</dt>
    <dd>Tons of improvements in the documentation extension, lots of
    cleanup, better HTML generation, better footnotes, what's not to like!
    </dd>
    <dt>20 Feb 2007</dt>
    <dd>Lots of stuff has happened; see the change log for details.
    </dd>
    <dt>5 June 2006</dt>
    <dd>More tweaking of block structure processing and paragraph marking. In every day and in every way, it's getting better and better.
    </dd>
    <dt>22 May 2006</dt>
    <dd>Removed LML2 dependency for CL-Markdown and fixed some bugs!
    </dd>
    <dt>17 May 2006</dt>
    <dd>Updated with SBCL and Allegro support (son far only alisp)
    </dd>
    <dt>8 May 2006</dt>
    <dd>Created site.
    </dd>
</dl>

</div>
</div>

{include resources/footer.md}

   [1]: http://common-lisp.net/project/cl-containers/shared/metabang-2.png (metabang.com)
   [2]: http://www.metabang.com/ (metabang.com)
   [3]: #mailing-lists
   [4]: #downloads
   [5]: documentation/ (documentation link)
   [6]: #news
   [7]: changelog.html
   [8]: comparison-tests
   [9]: http://trac.common-lisp.net/cl-markdown
   [10]: http://trac.common-lisp.net/cl-markdown/newticket
   [11]: http://common-lisp.net/cgi-bin/mailman/listinfo/cl-markdown-announce
   [12]: http://common-lisp.net/cgi-bin/mailman/listinfo/cl-markdown-devel
   [13]: downloads
   [14]: http://www.cliki.net/asdf-install
   [15]: http://common-lisp.net/project/cl-markdown/cl-markdown_latest.tar.gz
   [16]: http://www.darcs.net/
   [17]: news
   [18]: http://common-lisp.net/project/cl-containers/shared/buttons/xhtml.gif (valid xhtml button)
   [19]: http://validator.w3.org/check/referer (xhtml1.1)
   [20]: http://common-lisp.net/project/cl-containers/shared/buttons/hacker.png (hacker emblem)
   [21]: http://www.catb.org/hacker-emblem/ (hacker)
   [22]: http://common-lisp.net/project/cl-containers/shared/buttons/lml2-powered.png (lml2 powered)
   [23]: http://lml2.b9.com/ (lml2 powered)
   [24]: http://common-lisp.net/project/cl-containers/shared/buttons/lambda-lisp.png (ALU emblem)
   [25]: http://www.lisp.org/ (Association of Lisp Users)
   [26]: http://common-lisp.net/project/cl-containers/shared/buttons/lisp-lizard.png (Common-Lisp.net)
   [27]: http://common-lisp.net/ (Common-Lisp.net)

