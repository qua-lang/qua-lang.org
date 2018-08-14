(deftemplate main-template
  (div ()
   (h1 () (attribute 'title))
   (div ()
    (attribute 'child 'medium-template))))

(deftemplate page-template
  (div ()
   (h1 () (a (:href "index.html") "Qua") " / " (attribute 'title))
   (div ()
    (attribute 'child 'medium-template))))

(deftemplate medium-template
  (div ()
   (h3 () (node-link))
   (p () (attribute 'byline))))

(deftemplate manual-template
  (div (:style "font-family: serif")
       (center () (h1 () (a (:href "index.html") "Qua") " Language Manual"))
       (div ()
            (attribute 'child 'section-template))))

(deftemplate section-template
  (div ()
   (h2 () (node-link))
   (attribute 'child 'item-template)))

(deftemplate item-template
  (div ()
   (h3 () (attribute 'title) " [" (attribute 'type) "]")
   (h4 () "Syntax")
   (tt () (attribute 'syntax))
   (h4 () "Description")
   (p () (attribute 'description))))

