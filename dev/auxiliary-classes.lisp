(in-package cl-markdown)

# ======================================================================
# ========================== MISC AUXILIARY CLASSES ====================
# ======================================================================

class HtmlStash :

    """
        This class is used for stashing HTML objects that we extract
        in the beginning and replace with place-holders.
    """

    def __init__ (self) :
        self.html_counter = 0 # for counting inline html segments
        self.rawHtmlBlocks=[]
    
    def store(self, html) :
        """
           Saves an HTML segment for later reinsertion.  Returns a
           placeholder string that needs to be inserted into the
           document.

           @param html: an html segment
           @returns : a placeholder string
           
        """
        self.rawHtmlBlocks.append(html)            
        placeholder = HTML_PLACEHOLDER % self.html_counter
        self.html_counter += 1
        return placeholder


class BlockGuru :

    def _findHead(self, lines, fn, allowBlank=0) :

        """
           Functional magic to help determine boundaries of indented
           blocks.

           @param lines: an array of strings
           @param fn: a function that returns a substring of a string
                      if the string matches the necessary criteria
           @param allowBlank: specifies whether it's ok to have blank
                      lines between matching functions
           @returns: a list of post processes items and the unused
                      remainder of the original list

        """

        items = [] 
        item = -1

        i = 0 # to keep track of where we are

        for line in lines :

            if not line.strip() and not allowBlank:
                return items, lines[i:]

            if not line.strip() and allowBlank:
                # If we see a blank line, this _might_ be the end
                i += 1

                # Find the next non-blank line
                for j in range(i, len(lines)) :
                    if lines[j].strip() :
                        next = lines[j]
                        break
                else :
                    # There is no more text => this is the end
                    break

                # Check if the next non-blank line is still a part of the list

                part = fn(next)

                if part :
                    items.append("")
                    continue
                else :
                    break # found end of the list

            part = fn(line)

            if part :
                items.append(part)
                i += 1
                continue
            else :
                return items, lines[i:]
        else :
            i += 1

        return items, lines[i:]


    def detabbed_fn(self, line) :
        """ An auxiliary method to be passed to _findHead """
        m = RE.regExp['tabbed'].match(line)
        if m:
            return m.group(4)
        else :
            return None


    def detectTabbed(self, lines) :

        return self._findHead(lines, self.detabbed_fn,
                              allowBlank = 1)


def print_error(string):
    """
    Print an error string to stderr
    """
    sys.stderr.write(string +'\n')
    

def dequote(string) :
    """ Removes quotes from around a string """
    if ( ( string.startswith('"') and string.endswith('"'))
         or (string.startswith("'") and string.endswith("'")) ) :
        return string[1:-1]
    else :
        return string
