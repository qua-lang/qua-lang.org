(load "lisp/tcmplr.lisp")
(def #'define-node-type #'tcmplr-define-node-type)
(def #'defnode #'tcmplr-define-node)
(def #'node #'tcmplr-make-node)
(def #'hyper #'tcmplr-make-anchor)
(def #'deftemplate #'tcmplr-define-template)
(def #'associate-template #'tcmplr-associate-template)
(def #'node-field #'tcmplr-make-node-field-template)
(def #'file #'tcmplr-write-file)
(load "lisp/content.lisp")
(load "lisp/template.lisp")
(load "lisp/site.lisp")
