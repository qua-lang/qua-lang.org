(deftemplate qua-hub-manual-template-large
  (div ()
       (center () (h1 () (a (:href "index.html") "Qua") " Lisp Manual"))
       (div ()
            (node-field 'child 'default))))

(associate-template +qua-hub-manual-page+ 'default qua-hub-manual-template-large)

(deftemplate qua-hub-section-template-medium
  (div ()
       (h2 () (node-field 'title))
       (node-field 'child 'default)))

(associate-template +qua-hub-section+ 'default qua-hub-section-template-medium)

(deftemplate qua-hub-manual-operator-template-medium
  (div ()
       (h3 () (node-field 'title) (node-field 'type-name))
       (h4 () "Syntax")
       (em () (node-field 'syntax))
       (h4 () "Description")
       (div () (node-field 'description 'default))))

(associate-template +qua-hub-manual-operator+ 'default qua-hub-manual-operator-template-medium)

(deftemplate qua-hub-paragraph-template-medium
  (p () (node-field 'content)))

(associate-template +qua-hub-paragraph+ 'default qua-hub-paragraph-template-medium)

(deftemplate qua-hub-template-large
  (div ()
       (h1 () (node-field 'title))
       (div ()
            (node-field 'child 'medium))))

(associate-template +qua-hub-main-page+ 'large qua-hub-template-large)

(deftemplate qua-hub-page-template-large
  (div ()
       (h1 () (a (:href "index.html") "Qua") " / " (node-field 'title))
       (div ()
            (node-field 'child 'medium))))

(associate-template +qua-hub-page+ 'large qua-hub-page-template-large)

(defun page-link ()
  (a (:href (node-field 'page-name)) (node-field 'title)))

(deftemplate qua-hub-page-template-medium
  (div ()
       (h3 () (page-link))
       (p () (node-field 'byline))))

(associate-template +qua-hub-page+ 'medium qua-hub-page-template-medium)

