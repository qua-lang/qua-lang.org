;;;;; TCMPLR Template Compiler Lisp API

(defconstant +tcmplr+ (node:require "./js/tcmplr.js"))

;;;; Storage

(defconstant +tcmplr-store+ (js-new (.Store +tcmplr+)))

(defun tcmplr-get-node (reference)
  (@get_node +tcmplr-store+ reference))

(defun tcmplr-put-node (reference node)
  (@put_node +tcmplr-store+ reference node))

;;;; Nodes

(defconstant +tcmplr-node+ (.NODE +tcmplr+))

(defmacro tcmplr-define-node-type (name . opt-supertype-name)
  (list #'def name (list @make_node_type +tcmplr+
                         (symbol-name name)
                         (optional opt-supertype-name +tcmplr-node+))))

(defun tcmplr-make-node (type reference fields)
  (@make_node +tcmplr+ type reference fields))

(deffexpr tcmplr-anonymous-node (type . field-specs) env
  (tcmplr-make-node
   (eval type env)
   "anon"
   (tcmplr-parse-field-specs field-specs env)))

(deffexpr tcmplr-define-node (reference-form type . field-specs) env
  (let ((reference (tcmplr-parse-reference reference-form)))
    (tcmplr-put-node reference
                     (tcmplr-make-node
                      (eval type env)
                      reference
                      (tcmplr-parse-field-specs field-specs env)))))

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

(defun tcmplr-make-anchor (reference-form . opt-title)
  (@make_titled_anchor +tcmplr+
                       (tcmplr-parse-reference reference-form)
                       (optional opt-title #null)))

(defun tcmplr-make-immediate-anchor (node)
  (@make_immediate_anchor +tcmplr+ node))

;;;; References

;;; Syntax:
;;; (symbol . symbol) --- path with 1 element and fragment
;;; (symbol)          --- path with 1 element and no fragment

(defun tcmplr-make-reference (path fragment)
  (js-new (.Reference +tcmplr+) path fragment))

(defun tcmplr-parse-reference (form)
  (let ((path (js-array (symbol-name (car form)))))
    (tcmplr-make-reference path (if (nil? (cdr form))
                                    #null
                                  (symbol-name (cdr form))))))

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

;;;; Processing

(defun tcmplr-render (reference template)
  (let ((node (tcmplr-get-node reference)))
    (@render +tcmplr+ +tcmplr-store+ node template)))

(defun tcmplr-write-file (reference-form template)
  (let* ((reference (tcmplr-parse-reference reference-form))
         (file-name (js-get (.path reference) 0)))
    (load (+ "content/" file-name ".lisp"))
    (node:write-file-sync (+ "docs/" file-name ".html")
                          (tcmplr-render reference template))))
