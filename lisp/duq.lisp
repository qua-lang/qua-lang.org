;;;;; Duq: Static Site Generator for Qua-lang.org

;; The NPM package "escape-html" consists of a single function that
;; escapes characters unsafe for HTML.  Using #' we bind it in the
;; function namespace.
(def #'duq-escape-html (node:require "escape-html"))

;;;; Nodes

(defstruct duq-node
  id
  ;; map of attr-name -> list of attr-values
  attributes)

(defun duq-make-node (id attributes)
  (make-instance 'duq-node :id id :attributes attributes))

(deffexpr duq-define-node (id . attributes-list) env
  (eval (list #'def id
              (duq-make-node (symbol-name id)
                             (duq-parse-attributes attributes-list env)))
        env))

(defun duq-parse-attributes (attributes-list env)
  (let ((dict (js-object)))
    (for-each (lambda ((attr-name . attr-values))
                (js-set dict (symbol-name attr-name)
                        (map (lambda (val)
                               (eval val env))
                             attr-values)))
              attributes-list)
    dict))

;;;; Rendering

(defun duq-out (output string)
  (@push output string))

(defgeneric duq-render-template (template node output))

(defmethod duq-render-template ((s string) #ign output)
  (duq-out output (duq-escape-html s)))

(defstruct duq-html-tag
  name
  attributes ; name -> expr
  children)

(defmethod duq-render-template ((tag duq-html-tag) node output)
  (duq-out output (+ "<" (.name tag)))
  (duq-render-attributes tag node output)
  (duq-out output ">")
  (duq-render-children tag node output)
  (duq-out output (+ "</" (.name tag) ">\n")))

(defun duq-render-attributes (tag node output)
  (let ((attributes (.attributes tag)))
    (for-each (lambda (key)
                (duq-out output (+ " " key "=\""))
                (duq-render-template (js-get attributes key) node output)
                (duq-out output "\""))
              (@keys $Object attributes))))

(defun duq-render-children (tag node output)
  (for-each (lambda (child)
              (duq-render-template child node output))
            (.children tag)))

(defun duq-render-file (node template)
  (def output (js-array))
  (duq-render-template template node output)
  (node:write-file-sync (+ "docs/" (.id node) ".html")
                        (@join output "")))

;;;; HTML Tags

(defun duq-html-tag-definer (name)
  (vau (attributes . children) env
    (flet ((my-eval (obj) (eval obj env)))
      (make-instance 'duq-html-tag
                     :name name
                     :attributes (plist-to-js-object (map #'my-eval attributes))
                     :children (map #'my-eval children)))))

(deffexpr duq-define-html-tags names env
  (map (lambda (name)
         (eval (list #'def
                     (to-fun-sym name)
                     (duq-html-tag-definer (symbol-name name)))
               env))
       names))

(duq-define-html-tags
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

(defstruct duq-node-href)
(defmethod duq-render-template ((href duq-node-href) node output)
  (duq-out output (+ (.id node) ".html")))
(defun duq-node-href () (make-instance 'duq-node-href))

(defstruct duq-node-id)
(defmethod duq-render-template ((id duq-node-id) node output)
  (duq-out output (.id node)))
(defun duq-node-id () (make-instance 'duq-node-id))

(defun duq-find-attr (node attr default)
  (let ((attrs (.attributes node))
        (name (symbol-name attr)))
    (if (own-property? attrs name)
        (js-get attrs name)
      default)))

(defstruct duq-node-title)
(defmethod duq-render-template ((title duq-node-title) node output)
  (duq-out output (.id node)))
(defun duq-node-title () (make-instance 'duq-node-title))

(defconstant +duq-node-link+ (a (:href (duq-node-href))
                                (duq-node-title)))

(defun duq-node-link () +duq-node-link+)

(defstruct duq-attribute
  name
  template)
(defun duq-attribute (attribute . opt-template)
  (make-instance 'duq-attribute
                 :name (symbol-name attribute)
                 :template (optional opt-template)))
(defmethod duq-render-template ((attr duq-attribute) node output)
  (let ((attr-name (.name attr))
        (attr-template (.template attr))
        (node-attributes (.attributes node)))
    (if (own-property? node-attributes attr-name)
        (let ((attr-values (js-get node-attributes attr-name)))
          (for-each (lambda (val)
                      (typecase val
                                (string
                                 (duq-out output val))
                                (symbol
                                 (duq-render-template (eval attr-template (the-environment))
                                                      (eval val (the-environment))
                                                      output))))
                    attr-values))
      (duq-out output ""))))
          
