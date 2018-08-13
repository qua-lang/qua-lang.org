(defstruct html-tag
  name
  children)

(defun html-make-tag* (name children)
  (the string name)
  (make-instance 'html-tag :name name :children children))

(defun join-list (list . opt-char)
  (@join (list-to-js-array list) (optional opt-char "")))

(defmethod doqu-render ((fob html-tag))
  (join-list (list (+ "<" (.name fob) ">")
                   (join-list (map (lambda (child)
                                     (doqu-render child))
                                   (.children fob)))
                   (+ "</" (.name fob) ">\n"))))

(defun html-make-tag (name)
  (lambda (() . children)
    (html-make-tag* name children)))

(deffexpr define-html-tags names env
  (map (lambda (name)
         (eval (list #'def
                     (to-fun-sym name)
                     (html-make-tag (symbol-name name)))
               env))
       names))

(define-html-tags
  a
  body
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
  span
  strong)
