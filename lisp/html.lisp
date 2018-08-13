(defstruct html-tag
  name
  attrs
  children)

(defun html-make-tag* (name attrs children)
  (the string name)
  (the js-object attrs)
  (make-instance 'html-tag :name name :attrs attrs :children children))

(defun join-list (list . opt-char)
  (@join (list-to-js-array list) (optional opt-char "")))

(defmethod doqu-render ((fob html-tag))
  (join-list (list (+ "<" (.name fob) " " (html-render-attrs fob) ">")
                   (join-list (map (lambda (child)
                                     (doqu-render child))
                                   (.children fob)))
                   (+ "</" (.name fob) ">\n"))))

(defun html-render-attrs (tag)
  (@join (map (lambda (key)
                (+ key "=\"" (js-get (.attrs tag) key) "\""))
              (@keys $Object (.attrs tag)))
         ""))

(defun html-make-tag (name)
  (vau (attrs-plist . children) env
    (flet ((my-eval (obj) (eval obj env)))
          (html-make-tag* (my-eval name)
                          (plist-to-js-object (map #'my-eval attrs-plist))
                          (map #'my-eval children)))))

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
  center
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
