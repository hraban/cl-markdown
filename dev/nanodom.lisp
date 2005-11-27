(in-package cl-markdown)

#|
# ======================================================================
# ========================== NANODOM ===================================
# ======================================================================
#
# The three classes below implement some of the most basic DOM
# methods.  I use this instead of minidom because I need a simpler
# functionality and do not want to require additional libraries.
#
# Importantly, NanoDom does not do normalization, which is what we
# want. It also adds extra white space when converting DOM to string
#
|#

(defclass* document ()
  ((document-element nil a)
   (entities (make-container 'simple-associative-container) r)))

;;; ---------------------------------------------------------------------------

(defmethod append-children ((self document) child)
  (setf (document-element self) child
        (parent child) self)
  (empty! (entities self)))

;;; ---------------------------------------------------------------------------

(defmethod create-element ((self document) tag)
  (let ((el (element tag)))
    (setf (doc el) self)
    el))

;;; ---------------------------------------------------------------------------

(defmethod create-text-node ((self document) text)
  (let ((node (text-node text)))
    (setf (doc node) self)
    node))

;;; ---------------------------------------------------------------------------

(defmethod create-entity-reference ((self document) entity)
  (unless (find-element (entities self) entity)
    self.entities[entity] = EntityReference(entity))
  
  return self.entities[entity] 
  )

;;; ---------------------------------------------------------------------------

(defmethod to-xml ((self document))
  (to-xml (document-element self)))

;;; ---------------------------------------------------------------------------

(defmethod normalize-entities ((self document) text)
  (let ((pairs '(("&" . "&amp;")
                  ("<" . "&lt;")
                  (">" . "&gt;")
                  ("\"" . "&quot;"))))
    (loop for (old . new) in pairs do
          (setf text (xxx-substitute old new text)))
            
    (values text)))

;;; ---------------------------------------------------------------------------

(defmethod unlink ((self document))
  (unlink (document-element self))
  (setf (document-element self) nil))


;;; ---------------------------------------------------------------------------
;;; Element
;;; ---------------------------------------------------------------------------

(defclass* element ()
  ((parent nil ia)
   (type "element" ia)
   (node-name nil ia)
   (attributes (make-container 'list-container) r)
   (attribute-values (make-container 'simple-associative-container) r)
   (child-nodes (make-container 'list-container) r)))

;;; ---------------------------------------------------------------------------

(defmethod unlink ((self element))
  (iterate-elements 
   (child-nodes self)
   (lambda (child)
     (when (eq (type-of child) 'element)
       (unlink child))))
  (empty! (child-nodes self)))

;;; ---------------------------------------------------------------------------

(defmethod set-attribute ((self element) attr value)
  (unless (find-item (attributes self) attr)
    (insert-item (attributes self) attr))

  (setf (item-at (attribute-values self) attr) value))

;;; ---------------------------------------------------------------------------

(defmethod append-child ((self element) child)
  (insert-item (child-nodes self) child)
  (setf (parent child) self))

;;; ---------------------------------------------------------------------------

(defmethod handle-attributes ((self element))
  (values))

;;; ---------------------------------------------------------------------------

(defmethod to-xml ((self element))
  (when *enable-attributes*
    (iterate-elements (child-nodes self) #'handle-attributes))
    
  (let ((buffer "")
        (node-name (node-name self)))
    (flet ((buffer-append (string)
             (setf buffer (concatenate 'string buffer string))))
      (cond ((find node-name '(h1 h2 h3 h4))
             (buffer-append "\n"))
            ((find node-name '(li))
             (buffer-append "\n")))
        
      (buffer-append "<")
      (buffer-append (symbol-name node-name))
      (iterate-element 
       (attributes self)
       (lambda (attribute)
         value = self.attribute_values[attr]
         value = self.doc.normalizeEntities(value)
         buffer += ' %s="%s"' % (attr, value) 
        if self.childNodes :
            buffer += ">"
            for child in self.childNodes :
                buffer += child.toxml()
            if self.nodeName == 'p' :
                buffer += "\n"
            elif self.nodeName == 'li' :
                buffer += "\n "
            buffer += "</%s>" % self.nodeName
        else :
            buffer += "/>"
        if self.nodeName in ['p', 'li', 'ul', 'ol',
                             'h1', 'h2', 'h3', 'h4'] :
            buffer += "\n"
        return buffer

        )))
    ))

;;; ---------------------------------------------------------------------------
;;; text-node
;;; ---------------------------------------------------------------------------

(defclass* text-node ()
  ((parent nil ia)
   (type "text" a)
   (value nil ia)
   ;; {@id=123}
   (attr-reg-exp (ppcre:create-scanner "\{@([^\}]*)=([^\}]*)}") r))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((self text-node) &key text)
  )

;;; ---------------------------------------------------------------------------

(defmethod attribute-callback ((self text-node) match) 
  (set-attribute (parent self) match.group(1) match.group(2)))

(defmethod handle-attributes ((self text-node))
        self.value = self.attrRegExp.sub(self.attributeCallback, self.value)
        )

(defmethod to-xml ((self text-node))
  (let ((text (values self)))

    if not text.startswith(HTML_PLACEHOLDER_PREFIX):
            if self.parent.nodeName == "p" :
                text = text.replace("\n", "\n   ")
            elif (self.parent.nodeName == "li"
                  and self.parent.childNodes[0]==self):
                text = "\n     " + text.replace("\n", "\n     ")

        text = self.doc.normalizeEntities(text)
        return text
        ))

;;; ---------------------------------------------------------------------------
;;; entity-reference:
;;; ---------------------------------------------------------------------------


(defclass* entity-reference ()
  ((type 'entity-ref r)
   (entity nil ia)))

;;; ---------------------------------------------------------------------------

(defmethod handle-attributes ((self entity-reference))
  (values))

;;; ---------------------------------------------------------------------------

(defmethod to-xml ((self entity-reference))
  (values (concatenate 'string "&" (entity self) ";")))
