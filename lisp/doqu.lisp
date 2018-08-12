;;;;; Doqu: Static Site Generator for Qua-lang.org

;;; The NPM package "escape-html" consists of a single function that
;;; escapes characters unsafe for HTML.  Using #' we bind it in the
;;; function namespace.
(def #'escape-html (node-require "escape-html"))

;;;; Nodes

(defstruct doqu-node
  id
  rel
  rev)

(defun doqu-make-node (id rel rev)
  (the string id)
  (the js-object rel)
  (the js-object rev)
  (make-instance 'doqu-node :id id :rel rel :rev rev))

(deffexpr doqu-define-node (id rel . opt-rev) env
  (let ((node (doqu-make-node (symbol-name id)
                              (doqu-parse-attributes rel)
                              (if (nil? opt-rev)
                                  (js-object)
                                (doqu-parse-attributes (car opt-rev))))))
    (eval (list #'def id node) env)))

(defun doqu-parse-attributes (rel-or-rev . lists)
  (let ((dict (js-object)))
    (for-each (lambda ((attr-name . attr-values))
                (js-set dict (symbol-name attr-name) attr-values))
              lists)
    dict))

;;;; Templates

(deffexpr doqu-define-template (name expr) env
  )

;;;; Rendering

(defgeneric doqu-render (obj))

(defmethod doqu-render ((obj string))
  string)

(defun doqu-render-attribute (link-relation-type))

;;; The node currently being rendered.
(defdynamic *doqu-node*)

(defun doqu-render-site (site))
