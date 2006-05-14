(in-package #:cl-markdown)

#|
# ======================================================================
# ========================== POST-PROCESSORS ===========================
# ======================================================================

# Markdown also allows post-processors, which are similar to
# preprocessors in that they need to implement a "run" method.  Unlike
# pre-processors, they take a NanoDom document as a parameter and work
# with that.
#
# There are currently no standard post-processors, but the footnote
# extension below uses one.
|#