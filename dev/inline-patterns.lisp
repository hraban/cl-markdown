(in-package cl-markdown)

# ======================================================================
# ========================== INLINE PATTERNS ===========================
# ======================================================================
#
# Inline patterns such as *emphasis* are handled by means of auxiliary
# objects, one per pattern.  Each pattern object uses a single regular
# expression and needs support the following methods:
#
#   pattern.getCompiledRegExp() - returns a regular expression
#
#   pattern.handleMatch(m, doc) - takes a match object and returns
#                                 a NanoDom node (as a part of the provided
#                                 doc) or None
#
# All of python markdown's built-in patterns subclass from BasePatter,
# but you can add additional patterns that don't.
#

# Also note that all the regular expressions used by inline must
# capture the whole block.  For this reason, they all start with
# '^(.*)' and end with '(.*)!'.  In case with built-in expression
# BasePattern takes care of adding the "^(.*)" and "(.*)!".

# Finally, the order in which regular expressions are applied is very
# important - e.g. if we first replace http://.../ links with <a> tags
# and _then_ try to replace inline html, we would end up with a mess.
# So, we apply the expressions in the following order:
#
#        * escape and backticks have to go before everything else, so
#          that we can preempt any markdown patterns by escaping them.
#
#        * then we handle auto-links (must be done before inline html)
#        
#        * then we handle inline HTML.  At this point we will simply
#          replace all inline HTML strings with a placeholder and add
#          the actual HTML to a hash.
#
#        * then inline images (must be done before links)
#
#        * then bracketed links, first regular then reference-style
#                
#        * finally we apply strong and emphasis




NOBRACKET = r'[^\]\[]*'
BRK = ( r'\[('
        + (NOBRACKET + r'(\['+NOBRACKET)*6
        + (NOBRACKET+ r'\])*'+NOBRACKET)*6
        + NOBRACKET + r')\]' )

BACKTICK_RE = r'\`([^\`]*)\`'                    # `e= m*c^2`
DOUBLE_BACKTICK_RE =  r'\`\`(.*)\`\`'            # ``e=f("`")``
ESCAPE_RE = r'\\(.)'                             # \<
EMPHASIS_RE = r'\*([^\*]*)\*'                    # *emphasis*
EMPHASIS_2_RE = r'_([^_]*)_'                     # _emphasis_
LINK_RE = BRK + r'\s*\(([^\)]*)\)'               # [text](url)
LINK_ANGLED_RE = BRK + r'\s*\(<([^\)]*)>\)'      # [text](<url>)
IMAGE_LINK_RE = r'\!' + BRK + r'\s*\(([^\)]*)\)' # ![alttxt](http://x.com/)
REFERENCE_RE = BRK+ r'\s*\[([^\]]*)\]'       # [Google][3]
IMAGE_REFERENCE_RE = r'\!' + BRK + '\s*\[([^\]]*)\]' # ![alt text][2]
NOT_STRONG_RE = r'( \* )'                        # stand-alone * or _
STRONG_RE = r'\*\*(.*)\*\*'                      # **strong**
STRONG_2_RE = r'__([^_]*)__'                     # __strong__
STRONG_EM_RE = r'\*\*\*([^_]*)\*\*\*'            # ***strong***
STRONG_EM_2_RE = r'___([^_]*)___'                # ___strong___
AUTOLINK_RE = r'<(http://[^>]*)>'                # <http://www.123.com>
AUTOMAIL_RE = r'<([^> ]*@[^> ]*)>'               # <me@example.com>
HTML_RE = r'(\<[^\>]*\>)'                        # <...>
ENTITY_RE = r'(&[\#a-zA-Z0-9]*;)'                # &amp;

class BasePattern:

    def __init__ (self, pattern) :
        self.pattern = pattern
        self.compiled_re = re.compile("^(.*)%s(.*)$" % pattern, re.DOTALL)

    def getCompiledRegExp (self) :
        return self.compiled_re

class SimpleTextPattern (BasePattern) :

    def handleMatch(self, m, doc) :
        return doc.createTextNode(m.group(2))

class SimpleTagPattern (BasePattern):

    def __init__ (self, pattern, tag) :
        BasePattern.__init__(self, pattern)
        self.tag = tag

    def handleMatch(self, m, doc) :
        el = doc.createElement(self.tag)
        el.appendChild(doc.createTextNode(m.group(2)))
        return el

class BacktickPattern (BasePattern):

    def __init__ (self, pattern):
        BasePattern.__init__(self, pattern)
        self.tag = "code"

    def handleMatch(self, m, doc) :
        el = doc.createElement(self.tag)
        text = m.group(2).strip()
        text = text.replace("&", "&amp;")
        el.appendChild(doc.createTextNode(text))
        return el


class DoubleTagPattern (SimpleTagPattern) :

    def handleMatch(self, m, doc) :
        tag1, tag2 = self.tag.split(",")
        el1 = doc.createElement(tag1)
        el2 = doc.createElement(tag2)
        el1.appendChild(el2)
        el2.appendChild(doc.createTextNode(m.group(2)))
        return el1


class HtmlPattern (BasePattern):

    def handleMatch (self, m, doc) :
        place_holder = self.stash.store(m.group(2))
        return doc.createTextNode(place_holder)

    
class LinkPattern (BasePattern):

    def handleMatch(self, m, doc) :
        el = doc.createElement('a')
        el.appendChild(doc.createTextNode(m.group(2)))

        parts = m.group(9).split()
        # We should now have [], [href], or [href, title]
        
        if parts :
            el.setAttribute('href', parts[0])
        else :
            el.setAttribute('href', "")

        if len(parts) > 1 :
            # we also got a title
            title = " ".join(parts[1:]).strip()
            title = dequote(title) #.replace('"', "&quot;")
            el.setAttribute('title', title)

        return el


class ImagePattern (BasePattern):

    def handleMatch(self, m, doc) :
        el = doc.createElement('img')
        src_parts = m.group(9).split()
        el.setAttribute('src', src_parts[0])
        if len(src_parts) > 1 :
            el.setAttribute('title', dequote(" ".join(src_parts[1:])))

        if ENABLE_ATTRIBUTES :
            text = doc.createTextNode(m.group(2))
            el.appendChild(text)
            text.handleAttributes()
            truealt = text.value
            el.childNodes.remove(text)
        else:
            truealt = m.group(2)
        el.setAttribute('alt', truealt)
        
        return el
    
class ReferencePattern (BasePattern):

    def handleMatch(self, m, doc) :

        if m.group(9) :
            id = m.group(9).lower()
        else :
            # if we got something like "[Google][]"
            # we'll use "google" as the id
            id = m.group(2).lower()

        if not self.references.has_key(id) : # ignore undefined refs
            return None

        href, title = self.references[id]
        text = m.group(2)
        return self.makeTag(href, title, text, doc)
        
    def makeTag(self, href, title, text, doc) :
        el = doc.createElement('a')
        el.setAttribute('href', href)
        if title :
            el.setAttribute('title', title)
        el.appendChild(doc.createTextNode(text))
        return el


class ImageReferencePattern (ReferencePattern) :

    def makeTag(self, href, title, text, doc) :
        el = doc.createElement('img')
        el.setAttribute('src', href)
        if title :
            el.setAttribute('title', title)
        el.setAttribute('alt', text)
        return el
    

class AutolinkPattern (BasePattern) :

    def handleMatch(self, m, doc) :
        el = doc.createElement('a')
        el.setAttribute('href', m.group(2))
        el.appendChild(doc.createTextNode(m.group(2)))
        return el

class AutomailPattern (BasePattern) :

    def handleMatch(self, m, doc) :
        el = doc.createElement('a')
        email = m.group(2)
        for letter in email:
            entity = doc.createEntityReference("#%d" % ord(letter))
            el.appendChild(entity)                

        mailto = "mailto:" + m.group(2)
        mailto = "".join(['&#%d;' % ord(letter) for letter in mailto])
        el.setAttribute('href', mailto)
        return el



ESCAPE_PATTERN          = SimpleTextPattern(ESCAPE_RE)
NOT_STRONG_PATTERN      = SimpleTextPattern(NOT_STRONG_RE)

BACKTICK_PATTERN        = BacktickPattern(BACKTICK_RE)
DOUBLE_BACKTICK_PATTERN = BacktickPattern(DOUBLE_BACKTICK_RE)
STRONG_PATTERN          = SimpleTagPattern(STRONG_RE, 'strong')
STRONG_PATTERN_2        = SimpleTagPattern(STRONG_2_RE, 'strong')        
EMPHASIS_PATTERN        = SimpleTagPattern(EMPHASIS_RE, 'em')
EMPHASIS_PATTERN_2      = SimpleTagPattern(EMPHASIS_2_RE, 'em')

STRONG_EM_PATTERN       = DoubleTagPattern(STRONG_EM_RE, 'strong,em')
STRONG_EM_PATTERN_2     = DoubleTagPattern(STRONG_EM_2_RE, 'strong,em')

LINK_PATTERN            = LinkPattern(LINK_RE)
LINK_ANGLED_PATTERN     = LinkPattern(LINK_ANGLED_RE)
IMAGE_LINK_PATTERN      = ImagePattern(IMAGE_LINK_RE)
IMAGE_REFERENCE_PATTERN = ImageReferencePattern(IMAGE_REFERENCE_RE)
REFERENCE_PATTERN       = ReferencePattern(REFERENCE_RE)

HTML_PATTERN            = HtmlPattern(HTML_RE)
ENTITY_PATTERN          = HtmlPattern(ENTITY_RE)

AUTOLINK_PATTERN        = AutolinkPattern(AUTOLINK_RE)
AUTOMAIL_PATTERN        = AutomailPattern(AUTOMAIL_RE)
