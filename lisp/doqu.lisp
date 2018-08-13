;;;;; Doqu: Static Site Generator for Qua-lang.org

;;; The NPM package "escape-html" consists of a single function that
;;; escapes characters unsafe for HTML.  Using #' we bind it in the
;;; function namespace.
(def #'escape-html (node:require "escape-html"))

;;;; Nodes

(defstruct doqu-node
  id
  attrs)

(defun doqu-make-node (id attrs)
  (the string id)
  (the js-object attrs)
  (make-instance 'doqu-node :id id :attrs attrs))

(deffexpr doqu-define-node (id . attrs-list) env
  (let ((node (doqu-make-node (symbol-name id)
                              (doqu-parse-attributes attrs-list))))
    (eval (list #'def id node) env)))

(defun doqu-parse-attributes (attrs-list)
  (let ((dict (js-object)))
    (for-each (lambda ((attr-name . attr-values))
                (js-set dict (symbol-name attr-name) attr-values))
              attrs-list)
    dict))

;;;; Rendering

(defgeneric doqu-render (fob))
(defmethod doqu-render ((fob string))
  (escape-html fob))
(defmethod doqu-render ((fob nil))
  "")
(defmethod doqu-render ((fob cons))
  (@join (list-to-array (map #'doqu-render fob)) ""))

;;;; Misc

(defun doqu-attribute #ign
  "ATTR")

(defun doqu-node-link (node)
  "LINK")
