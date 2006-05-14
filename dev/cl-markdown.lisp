(in-package #:cl-markdown)

;;; ---------------------------------------------------------------------------
;;; "entry" point
;;; ---------------------------------------------------------------------------

(defun markdown (text)
  (str (make-instance 'markdown :text text)))

#|
# ======================================================================
# ========================== CORE MARKDOWN =============================
# ======================================================================

# This stuff is ugly, so if you are thinking of extending the syntax,
# see first if you can do it via pre-processors, post-processors,
# inline patterns or a combination of the three.
|#

(defparameter *core-patterns-patterns*
  (make-container 'simple-associative-container
                  :initial-contents
                  '(                    ; a title
                    header        "(#*)([^#]*)(#*)"
                                        ; [Google]: http://www.google.com/
                    reference-def "(\ ?\ ?\ ?)\[([^\]]*)\]:\s*([^ ]*)(.*)"
                                        ; -----, =====, etc.
                    containsline  "([-]*)$|^([=]*)"
                                        ; 1. text
                    ol            "[ ]{0,3}[\d]*\.\s+(.*)"
                                        ; "* text"
                    ul            "[ ]{0,3}[*+-]\s+(.*)" 
                                        ; ***
                    isline1       "(\**)" 
                                        ; ---
                    isline2       "(\-*)"
                                        ; ___
                    isline3       "(\_*)"
                                        ; an indented line
                    tabbed        "((\t)|(    ))(.*)" 
                                        ; a quoted block ("> ...")
                    quoted        "> ?(.*)")))

;;; ---------------------------------------------------------------------------

(defclass* core-patterns ()
 "This classes is scheduled for getting removed as part of a refactoring effort."
 ((reg-exp (make-container 'simple-associative-container) r)))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((object core-patterns) &key)
  (iterate-key-value
   *core-patterns-patterns*
   (lambda (key re)
     (setf (item-at (reg-exp self) key) 
           (ppcre:create-scanner (format nil "^~A$" re)))))
  
  (setf (item-at (reg-exp self) 'contains-line)
        (ppcre:create-scanner "^([-]*)$|^([=]*)$")))


;;??
;; RE = CorePatterns()

;;; ---------------------------------------------------------------------------
;;; markdown
;;; ---------------------------------------------------------------------------

(defclass* markdown () 
  "Markdown formatter class for creating an html document from Markdown text."
  ((source nil ia)
   (block-guru (make-instance 'block-guru) a)
   (html-stash (make-instance 'html-stash) a)
   (registered-extensions (make-container 'list-container) r)
   (pre-processors (make-container 'list-container) r)
   (post-processors (make-container 'list-container) r)
   (inline-patterns (make-container 'list-container) r)
   (doc nil a)
   (top_element nil a)
   (references (make-container 'list-container) r)))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((self markdown) &key source)
  (loop for pre-processor-class in *pre-processors* do
        (insert-item (pre-processors self) 
                     (make-instance pre-processor-class)))
  
  (loop for class-name in *inline-patterns* do
        (insert-item (inline-patterns self) 
                     (make-instance class-name)))
  
  (reset self))

;;; ---------------------------------------------------------------------------

(defmethod register-Extension ((self markdown) extension)
  (append-item (registered-extensions self) extension))

;;; ---------------------------------------------------------------------------

(defmethod reset ((self markdown))
  "Resets all state variables so that we can starte with a new text."
  (empty! (references self))
  (setf (html-stash self) (make-instance 'html-stash))
  (iterate-elements
   (list (pre-processors self)
         (post-processors self)
         (inline-patterns self))
   (lambda (list)
     (iterate-elements 
      list
      (lambda (pp)
        (when (slot-exists-p pp 'html-stash)
          (setf (slot-value pp 'html-stash) (html-stash self)))
        (when (slot-exists-p pp 'references)
          (setf (slot-value pp 'references) (references self)))))))
  
  (iterate-elements (registered-extensions self) #'reset))

;;; ---------------------------------------------------------------------------

(defmethod _transform ((self markdown))
  "Transforms the Markdown text into a XHTML body document" 

  ;; Setup the document
  (setf (doc self) (make-instance 'document) 
        (top-element self) (create-element (doc self) 'span))
  (append-child (top-element self) (create-text-node (doc self) "\n"))
  (set-attributes (top-element self)  
  self.top_element.setAttribute('class', 'markdown')
        self.doc.appendChild(self.top_element)

        # Fixup the source text
        text = self.source.strip()
        text = text.replace("\r\n", "\n").replace("\r", "\n")
        text += "\n\n"
        text = text.expandtabs(TAB_LENGTH)

        # Split into lines and run the preprocessors that will work with
        # self.lines

        self.lines = text.split("\n")

        # Run the pre-processors on the lines
        for prep in self.preprocessors :
            self.lines = prep.run(self.lines)

        # Create a NanoDom tree from the lines and attach it to Document
        self._processSection(self.top_element, self.lines)

        # Not sure why I put this in but let's leave it for now.
        self.top_element.appendChild(self.doc.createTextNode('\n'))

        # Run the post-processors
        for postprocessor in self.postprocessors :
            postprocessor.run(self.doc)

        return self.doc    
        ))

;;; ---------------------------------------------------------------------------
 
(defmethod _processSection(self, parent_elem, lines,
                                 inList = 0, looseList = 0) :

        "Process a section of a source document, looking for high
           level structural elements like lists, block quotes, code
           segments, html blocks, etc.  Some those then get stripped
           of their high level markup (e.g. get unindented) and the
           lower-level markup is processed recursively.

           @param parent_elem: A NanoDom element to which the content
                               will be added
           @param lines: a list of lines
           @param inList: a level
           @returns: None
        "
        
        if not lines :
            return

        # Check if this section starts with a list, a blockquote or
        # a code block

        processFn = { 'ul' :     self._processUList,
                      'ol' :     self._processOList,
                      'quoted' : self._processQuote,
                      'tabbed' : self._processCodeBlock }
        
        for regexp in ['ul', 'ol', 'quoted', 'tabbed'] :
            m = RE.regExp[regexp].match(lines[0])
            if m :
                processFn[regexp](parent_elem, lines, inList)
                return

        # We are NOT looking at one of the high-level structures like
        # lists or blockquotes.  So, it's just a regular paragraph
        # (though perhaps nested inside a list or something else).  If
        # we are NOT inside a list, we just need to look for a blank
        # line to find the end of the block.  If we ARE inside a
        # list, however, we need to consider that a sublist does not
        # need to be separated by a blank line.  Rather, the following
        # markup is legal:
        #
        # * The top level list item
        #
        #     Another paragraph of the list.  This is where we are now.
        #     * Underneath we might have a sublist.
        #
        
        if inList :

            start, theRest = self._linesUntil(lines, (lambda line:
                             RE.regExp['ul'].match(line)
                             or RE.regExp['ol'].match(line)
                                              or not line.strip()))

            self._processSection(parent_elem, start,
                                 inList - 1, looseList = looseList)
            self._processSection(parent_elem, theRest,
                                 inList - 1, looseList = looseList)
            
                    
        else : # Ok, so it's just a simple block

            paragraph, theRest = self._linesUntil(lines, lambda line:
                                                 not line.strip())

            if len(paragraph) and paragraph[0].startswith('#') :
                m = RE.regExp['header'].match(paragraph[0])
                if m :
                    level = len(m.group(1))
                    h = self.doc.createElement("h%d" % level)
                    parent_elem.appendChild(h)
                    for item in self._handleInline(m.group(2)) :
                        h.appendChild(item)
                else :
                    print "We've got a problem header!"

            elif paragraph :

                list = self._handleInline("\n".join(paragraph))

                if ( parent_elem.nodeName == 'li'
                     and not (looseList or parent_elem.childNodes)):

                    #and not parent_elem.childNodes) :
                    # If this is the first paragraph inside "li", don't
                    # put <p> around it - append the paragraph bits directly
                    # onto parent_elem
                    el = parent_elem
                else :
                    # Otherwise make a "p" element
                    el = self.doc.createElement("p")
                    parent_elem.appendChild(el)

                for item in list :
                    el.appendChild(item)
                
            if theRest :
                theRest = theRest[1:]  # skip the first (blank) line

            self._processSection(parent_elem, theRest, inList)
            
)

;;; ---------------------------------------------------------------------------

(defmethod _processUList(self, parent_elem, lines, inList) :
        self._processList(parent_elem, lines, inList,
                         listexpr='ul', tag = 'ul')
        )

;;; ---------------------------------------------------------------------------

(defmethod _processOList(self, parent_elem, lines, inList) :
        self._processList(parent_elem, lines, inList,
                         listexpr='ol', tag = 'ol')
        )

;;; ---------------------------------------------------------------------------

(defmethod _processList(self, parent_elem, lines, inList, listexpr, tag) :
        "Given a list of document lines starting with a list item,
           finds the end of the list, breaks it up, and recursively
           processes each list item and the remainder of the text file.

           @param parent_elem: A dom element to which the content will be added
           @param lines: a list of lines
           @param inList: a level
           @returns: None
        "

        ul = self.doc.createElement(tag)  # ul might actually be '<ol>'
        parent_elem.appendChild(ul)

        looseList = 0

        # Make a list of list items
        items = [] 
        item = -1

        i = 0  # a counter to keep track of where we are

        for line in lines :

            loose = 0
            if not line.strip() :
                # If we see a blank line, this _might_ be the end of the list
                i += 1
                loose = 1
                
                # Find the next non-blank line
                for j in range(i, len(lines)) :
                    if lines[j].strip() :
                        next = lines[j]
                        break
                else :
                    # There is no more text => end of the list
                    break
                
                # Check if the next non-blank line is still a part of the list
                if ( RE.regExp[listexpr].match(next) or
                     RE.regExp['tabbed'].match(next) ):
                    items[item].append(line)
                    looseList = loose or looseList
                    continue
                else :
                    break # found end of the list

            # Now we need to detect list items (at the current level)
            # while also detabing child elements if necessary

            for expr in [listexpr, 'tabbed']:

                m = RE.regExp[expr].match(line)
                if m :
                    if expr == listexpr :  # We are looking at a new item
                        if m.group(1) : 
                            items.append([m.group(1)])
                            item += 1
                    elif expr == 'tabbed' :  # This line needs to be detabbed
                        items[item].append(m.group(4)) #after the 'tab'

                    i += 1
                    break
            else :
                items[item].append(line)  # Just regular continuation 
        else :
            i += 1

        # Add the dom elements
        for item in items :
            li = self.doc.createElement("li")
            ul.appendChild(li)

            self._processSection(li, item, inList + 1, looseList = looseList)

        # Process the remaining part of the section
        self._processSection(parent_elem, lines[i:], inList)
        )

;;; ---------------------------------------------------------------------------

(defmethod _linesUntil(self, lines, condition) :
        """ A utility function to break a list of lines upon the
            first line that satisfied a condition.  The condition
            argument should be a predicate function.
            """
        
        i = -1
        for line in lines :
            i += 1
            if condition(line) : break
        else :
            i += 1
        return lines[:i], lines[i:]

        )

;;; ---------------------------------------------------------------------------

(defmethod _processQuote(self, parent_elem, lines, inList) :
        "Given a list of document lines starting with a quote finds
           the end of the quote, unindents it and recursively
           processes the body of the quote and the remainder of the
           text file.

           @param parent_elem: DOM element to which the content will be added
           @param lines: a list of lines
           @param inList: a level
           @returns: None
        "

        dequoted = []
        i = 0
        for line in lines :
            m = RE.regExp['quoted'].match(line)
            if m :
                dequoted.append(m.group(1))
                i += 1
            else :
                break
        else :
            i += 1

        blockquote = self.doc.createElement('blockquote')
        parent_elem.appendChild(blockquote)

        self._processSection(blockquote, dequoted, inList)
        self._processSection(parent_elem, lines[i:], inList)
        )

;;; ---------------------------------------------------------------------------

(defmethod _processCodeBlock(self, parent_elem, lines, inList) :
        "
           Given a list of document lines starting with a code block
           finds the end of the block, puts it into the dom verbatim
           wrapped in ("<pre><code>") and recursively processes the 
           the remainder of the text file.

           @param parent_elem: DOM element to which the content will be added
           @param lines: a list of lines
           @param inList: a level
           @returns: None
        "

        detabbed, theRest = self.blockGuru.detectTabbed(lines)
                
        pre = self.doc.createElement('pre')
        code = self.doc.createElement('code')
        parent_elem.appendChild(pre)
        pre.appendChild(code)
        text = "\n".join(detabbed).rstrip()+"\n"
        text = text.replace("&", "&amp;")
        code.appendChild(self.doc.createTextNode(text))
        self._processSection(parent_elem, theRest, inList)
        )

;;; ---------------------------------------------------------------------------

(defmethod _handleInline(self,  line):
        "Transform a Markdown line with inline elements to an XHTML fragment.

        Note that this function works recursively: we look for a
        pattern, which usually splits the paragraph in half, and then
        call this function on the two parts.

        This function uses auxiliary objects called inline patterns.
        See notes on inline patterns above.

        @param item: A block of Markdown text
        @return: A list of NanoDomnodes
        "
        if not(line):
            return [self.doc.createTextNode(' ')]
        # two spaces at the end of the line denote a <br/>
        #if line.endswith('  '):
        #    list = self._handleInline( line.rstrip())
        #    list.append(self.doc.createElement('br'))
        #    return list
        #
        # ::TODO:: Replace with a preprocessor

        for pattern in self.inlinePatterns :
            list = self._applyPattern( line, pattern)
            if list: return list
                                         
        return [self.doc.createTextNode(line)]
        )

;;; ---------------------------------------------------------------------------

(defmethod _applyPattern(self,  line, pattern) :
           "Given a pattern name, this function checks if the line
            fits the pattern, creates the necessary elements and
            recursively calls _handleInline (via. _inlineRecurse)
        
        @param line: the text to be processed
        @param pattern: the pattern to be checked

        @returns: the appropriate newly created NanoDom element if the
                  pattern matches, None otherwise.
        "

        # match the line to pattern's pre-compiled reg exp.
        # if no match, move on.
        
        m = pattern.getCompiledRegExp().match(line)
        if not m :
            return None
        
        # if we got a match let the pattern make us a NanoDom node
        # if it doesn't, move on
        node = pattern.handleMatch(m, self.doc)
        if not node :
            return None

        # determine what we've got to the left and to the right

        left = m.group(1)      # the first match group
        left_list = self._handleInline(left)
        right = m.groups()[-1] # the last match group
        right_list = self._handleInline(right)

        # put the three parts together
        left_list.append(node)
        left_list.extend(right_list)
        
        return left_list
        )

;;; ---------------------------------------------------------------------------

(defmethod __str__ ((self markdown)) 
  "Return the document in XHTML format."
        try :
            doc = self._transform()
            xml = doc.toxml() 
        finally:
            doc.unlink()

        # Let's stick in all the raw html pieces

        for i in range(self.htmlStash.html_counter) :
            xml = xml.replace("<p>%s\n</p>" % (HTML_PLACEHOLDER % i),
                              self.htmlStash.rawHtmlBlocks[i] + "\n")
            xml = xml.replace(HTML_PLACEHOLDER % i,
                              self.htmlStash.rawHtmlBlocks[i])

        xml = xml.replace(FN_BACKLINK_TEXT, "&#8617;")

        # And return everything but the top level tag
        xml = xml.strip()[23:-7]

        return xml
        )

