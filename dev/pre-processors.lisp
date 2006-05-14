(in-package #:cl-markdown)


#|
# ======================================================================
# ========================== PRE-PROCESSORS ============================
# ======================================================================

# Preprocessors munge source text before we start doing anything too
# complicated.
#
# Each preprocessor implements a "run" method that takes a pointer to
# a list of lines of the document, modifies it as necessary and
# returns either the same pointer or a pointer to a new list.

|#

(defclass* basic-pre-processor () 
  ())

;;; ---------------------------------------------------------------------------

(defclass* header-pre-processor (basic-pre-processor) 
  "Replaces underlined headers with hashed headers to avoid the nead for lookahead later."
  ())

;;; ---------------------------------------------------------------------------

(defun empty-line-p (line)
  (zerop (size line)))

;;; ---------------------------------------------------------------------------

(defmethod run ((self header-pre-processor) lines)
  (let ((size (size lines)))
    (loop for i = 0 upto size
          unless (empty-line-p (nth-element lines i)) do
          (when (and (<= (1+ i) size)
                     (not (empty-line-p (nth-element lines (1+ i))))
                     (member (nth-element (nth-element lines (1+ i)) 0)
                             '(#\- #\=)))
            
        
        for i in range(len(lines)) :
            if not lines[i] :
                continue

            if (i+1 <= len(lines)
                  and lines[i+1]
                  and lines[i+1][0] in ['-', '=']) :

                underline = lines[i+1].strip()

                if underline == "="*len(underline) :
                    lines[i] = "# " + lines[i].strip()
                    lines[i+1] = ""
                elif underline == "-"*len(underline) :
                    lines[i] = "## " + lines[i].strip()
                    lines[i+1] = ""

        return lines

;;; ---------------------------------------------------------------------------

(defclass* line-pre-processor (basic-pre-processor)
    "Deals with HR lines (needs to be done before processing lists)"
    
    def run (self, lines) :
        for i in range(len(lines)) :
            if self._isLine(lines[i]) :
                lines[i] = "<hr/>"

        return lines

    def _isLine(self, block) :

        "Determines if a block should be replaced with an <HR>"

        if block.startswith("    ") : return 0  # a code block

        text = "".join([x for x in block if not x.isspace()])

        if len(text) <= 2 :
            return 0
        
        for pattern in ['isline1', 'isline2', 'isline3'] :
            m = RE.regExp[pattern].match(text)
            if (m and m.group(1)) :
                return 1
        else:

            return 0

;;; ---------------------------------------------------------------------------

(defclass* html-block-pre-processor (basic-pre-processor)
  "Removes html blocks from self.lines"

    def run (self, lines) :

        new_blocks = []

        text = "\n".join(lines)
              
        
        for block in text.split("\n\n") :

            if ( ( block.startswith("<!--") and block.rstrip().endswith(">") )
                 or
                 ( ( block.startswith("<") or block.startswith("\n<") )
                   and block.rstrip().endswith(">")
                   and ( is_block_level( block[1:].replace(">", " ")
                                         .split()[0].lower() ))) ) :
                
                new_blocks.append(
                    self.stash.store(block.strip()))
            else :
                new_blocks.append(block)

        return "\n\n".join(new_blocks).split("\n")

;;; ---------------------------------------------------------------------------

(defclass* reference-pre-processor (basic-pre-processor)

    def run (self, lines) :

        new_text = [];

        for line in lines:
            m = RE.regExp['reference-def'].match(line)
            if m:
                id = m.group(2).strip().lower()
                title = dequote(m.group(4).strip()) #.replace('"', "&quot;")
                self.references[id] = (m.group(3), title)
                                                             
            else :
                new_text.append(line)

        return new_text #+ "\n"
