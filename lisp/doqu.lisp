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
                              (doqu-parse-attributes attrs-list env))))
    (eval (list #'def id node) env)))

(defun doqu-parse-attributes (attrs-list env)
  (let ((dict (js-object)))
    (for-each (lambda ((attr-name . attr-values))
                (js-set dict (symbol-name attr-name)
                        (map (lambda (val)
                               (eval val env))
                             attr-values)))
              attrs-list)
    dict))

;;;; Rendering

(defgeneric doqu-render (fob))
(defmethod doqu-render ((fob object))
  (+ "" fob))
(defmethod doqu-render ((fob symbol))
  (symbol-name fob))
(defmethod doqu-render ((fob string))
  fob)
(defmethod doqu-render ((fob nil))
  "")
(defmethod doqu-render ((fob cons))
  (join-list (map #'doqu-render fob)))

;;;; Misc

(defun/env resolve (target) env
  (typecase target
            (symbol (the doqu-node (eval target env)))
            (string (escape-html target))
            (#t target)))

(defun doqu-attribute (object attr . opt-template)
  (the doqu-node object)
  (if (own-property? (.attrs object) (symbol-name attr))
      (let ((attr-values (js-get (.attrs object) (symbol-name attr))))
        (join-list (map (lambda (val)
                          (if (nil? opt-template)
                              (doqu-render (resolve val))
                            (doqu-render (funcall (the function (car opt-template)) (resolve val)))))
                        attr-values)))
      ""))

(defun doqu-node-link (node)
  (the doqu-node node)
  (a () (.id node)))
