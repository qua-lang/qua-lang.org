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
(def #'anchor-title #'tcmplr-make-anchor-title-template)
(def #'node-link #'tcmplr-make-node-link-template)
(def #'node-anchor #'tcmplr-make-node-anchor-template)
(def #'call-template #'tcmplr-make-call-template)
(def #'file #'tcmplr-write-file)

;;; Define website node types
(define-node-type +qua-hub-page+)
(define-node-type +qua-hub-section+)
(define-node-type +qua-hub-item+)
(define-node-type +qua-hub-paragraph+)
(define-node-type +qua-hub-weblink+)
(define-node-type +qua-hub-code-sample+)
(define-node-type +qua-hub-main-page+ +qua-hub-page+)
(define-node-type +qua-hub-manual-page+ +qua-hub-page+)
(define-node-type +qua-hub-manual-operator+ +qua-hub-item+)
(define-node-type +qua-hub-manual-syntax+ +qua-hub-item+)
(define-node-type +qua-hub-manual-naming-convention+ +qua-hub-item+)
(define-node-type +qua-hub-manual-constant+ +qua-hub-item+)
(define-node-type +qua-hub-manual-class+ +qua-hub-item+)
(define-node-type +qua-hub-manual-concept+ +qua-hub-item+)
(define-node-type +qua-hub-ref+ +qua-hub-item+)

;;; Convenience macros
(defmacro weblink props ;; title, url
  (list* #'node +qua-hub-weblink+ props))

(defmacro paragraph text
  (list #'node +qua-hub-paragraph+ (list* :text text)))

(defmacro html-sample text
  (list #'node +qua-hub-code-sample+ (list* :text text)))

(defmacro shell-sample text
  (list #'node +qua-hub-code-sample+ (list* :text text)))

(defmacro js-sample text
  (list #'node +qua-hub-code-sample+ (list* :text text)))

(defmacro define-manual (id . body)
  (list* #'defnode id +qua-hub-manual-page+
         body))

(defmacro define-section (id . body)
  (list* #'defnode id +qua-hub-section+
         body))

(defmacro define-manual-constant (id . body)
  (list* #'defnode id +qua-hub-manual-constant+
         '(:type-name "Constant")
         body))

(defmacro define-manual-class (id . body)
  (list* #'defnode id +qua-hub-manual-class+
         '(:type-name "Class")
         body))

(defmacro define-manual-concept (id . body)
  (list* #'defnode id +qua-hub-manual-concept+
         '(:type-name "Concept")
         body))

(defmacro define-manual-syntax (id . body)
  (list* #'defnode id +qua-hub-manual-syntax+
         '(:type-name "Syntax")
         body))

(defmacro define-manual-naming-convention (id . body)
  (list* #'defnode id +qua-hub-manual-naming-convention+
         '(:type-name "Naming Convention")
         body))

(defmacro define-manual-special (id . body)
  (list* #'defnode id +qua-hub-manual-operator+
         '(:type-name "Special Operator")
         body))

(defmacro define-manual-function (id . body)
  (list* #'defnode id +qua-hub-manual-operator+
         '(:type-name "Function")
         body))

(defmacro define-manual-fexpr (id . body)
  (list* #'defnode id +qua-hub-manual-operator+
         '(:type-name "Fexpr")
         body))

(defmacro define-manual-macro (id . body)
  (list* #'defnode id +qua-hub-manual-operator+
         '(:type-name "Macro")
         body))

;;; Load the content files.  Must also be added to lisp/site.lisp :'|
(load "content/index.lisp")
(load "content/intro.lisp")
;(load "content/tour.lisp")
(load "content/manual.lisp")
;(load "content/alpha.lisp")
(load "content/usage.lisp")
(load "content/ref.lisp")

;;; Load templates and rendering instructions
(load "lisp/template.lisp")
(load "lisp/site.lisp")
