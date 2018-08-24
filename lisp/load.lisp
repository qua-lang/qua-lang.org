;;; Load main file
(load "lisp/tcmplr.lisp")

;;; Do some renamings for comfort
(def #'define-node-type #'tcmplr-define-node-type)
(def #'defnode #'tcmplr-define-node)
(def #'node #'tcmplr-anonymous-node)
(def #'hyper #'tcmplr-make-anchor)
(def #'deftemplate #'tcmplr-define-template)
(def #'associate-template #'tcmplr-associate-template)
(def #'node-field #'tcmplr-make-node-field-template)
(def #'node-link #'tcmplr-make-node-link-template)
(def #'node-anchor #'tcmplr-make-node-anchor-template)
(def #'file #'tcmplr-write-file)

;;; Define website node types
(define-node-type +qua-hub-page+)
(define-node-type +qua-hub-section+)
(define-node-type +qua-hub-item+)
(define-node-type +qua-hub-paragraph+)
(define-node-type +qua-hub-main-page+ +qua-hub-page+)
(define-node-type +qua-hub-manual-page+ +qua-hub-page+)
(define-node-type +qua-hub-manual-operator+ +qua-hub-item+)
(define-node-type +qua-hub-ref+ +qua-hub-item+)

;;; Convenience macro for paragraph node
(defmacro paragraph text
  (list #'node +qua-hub-paragraph+ (list* :text text)))

;;; Load the content files.  Must also be added to lisp/site.lisp :'|
(load "content/index.lisp")
(load "content/intro.lisp")
(load "content/manual.lisp")
(load "content/alpha.lisp")
(load "content/usage.lisp")
(load "content/ref.lisp")

;;; Load templates and rendering instructions
(load "lisp/template.lisp")
(load "lisp/site.lisp")
