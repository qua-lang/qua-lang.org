;;;;; TCMPLR Template Compiler Lisp API

(defconstant +tcmplr+ (node:require "./js/tcmplr.js"))

;;;; Nodes

(defconstant +tcmplr-node+ (.NODE +tcmplr+))

(defmacro tcmplr-define-node-type (name . opt-supertype-name)
  (list #'def name (list @make_node_type +tcmplr+
                         (symbol-name name)
                         (optional opt-supertype-name +tcmplr-node+))))

(deffexpr tcmplr-make-node (type . field-specs) env
  (@make_node +tcmplr+
              (eval type env)
              "anon"
              (tcmplr-parse-field-specs field-specs env)))

(deffexpr tcmplr-define-node (id type . field-specs) env
  (eval (list #'def id (list @make_node +tcmplr+
                             type
                             (symbol-name id)
                             (tcmplr-parse-field-specs field-specs env)))
        env))

(defun tcmplr-parse-field-specs (field-specs env)
  (prog1 (def dict (js-object))
    (for-each (lambda ((field-name . field-values))
                (js-set dict (symbol-name field-name)
                        (list-to-js-array
                         (map (lambda (field-value)
                                (eval field-value env))
                              field-values))))
              field-specs)))

;;;; Anchors

(defun tcmplr-make-anchor (node-id . opt-title)
  (@make_titled_anchor +tcmplr+ (symbol-name node-id)
                       (optional opt-title #null)))

(defun tcmplr-make-immediate-anchor (node)
  (@make_immediate_anchor +tcmplr+ node))

;;;; Templates

(defmacro tcmplr-define-template (name form)
  (list #'def name (list @compile +tcmplr+ form)))

(defun tcmplr-make-node-field-template (field-name . opt-template-name)
  (js-new (.NodeFieldTemplate +tcmplr+)
          (symbol-name field-name)
          (symbol-name (optional opt-template-name 'inline))))

(defun tcmplr-make-tag-template (tag-name attrs children)
  (js-new (.TagTemplate +tcmplr+)
          tag-name
          attrs
          children))

(defun tcmplr-make-tag-template-operator (tag-name)
  (vau (attrs . children) env
    (flet ((my-eval (obj) (eval obj env)))
      (tcmplr-make-tag-template
       tag-name
       (plist-to-js-object (map #'my-eval attrs))
       (list-to-js-array (map #'my-eval children))))))

(deffexpr tcmplr-define-html-tags names env
  (map (lambda (name)
         (eval (list #'def
                     (to-fun-sym name)
                     (tcmplr-make-tag-template-operator (symbol-name name)))
               env))
       names))

(tcmplr-define-html-tags
  a
  body
  center
  code
  div
  em
  h1
  h2
  h3
  h4
  h5
  h6
  h7
  head
  html
  p
  pre
  span
  strong
  tt)

(defun tcmplr-associate-template (node-type template-name template)
  (js-set node-type (symbol-name template-name) template))

;;;; Rendering

(defun/env tcmplr-render (node template) env
  (@render +tcmplr+
           env
           node
           template))

(defun tcmplr-write-file (node template)
  (node:write-file-sync (+ "docs/" (.id node) ".html")
                        (tcmplr-render node template)))
