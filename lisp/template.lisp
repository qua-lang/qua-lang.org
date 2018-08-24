(deftemplate qua-hub-inline-template
  (a (:href (node-link)) (node-field 'title)))

(associate-template +tcmplr-node+ 'inline qua-hub-inline-template)

(deftemplate qua-hub-op-inline-template
  (a (:href (node-link) :style "text-transform: lowercase") (strong () (node-field 'title))))

(associate-template +qua-hub-manual-operator+ 'inline qua-hub-op-inline-template)

(deftemplate qua-hub-weblink-inline-template
  (a (:href (node-field 'url)) (node-field 'title)))

(associate-template +qua-hub-weblink+ 'inline qua-hub-weblink-inline-template)

(deftemplate qua-hub-manual-template-large
  (div ()
       (center () (h1 () (a (:href "index.html") "Qua") " Lisp Manual"))
       (blockquote () (node-field 'abstract 'default))
       (h3 () "Table of Contents")
       (ul ()
           (node-field 'child 'toc))
       (div ()
            (node-field 'child 'default))))

(associate-template +qua-hub-manual-page+ 'default qua-hub-manual-template-large)

(deftemplate qua-hub-section-template-medium
  (div ()
       (a (:id (node-anchor)) (h2 () (node-field 'title)))
       (node-field 'content 'default)
       (node-field 'child 'default)))

(associate-template +qua-hub-section+ 'default qua-hub-section-template-medium)

(deftemplate qua-hub-section-template-toc
  (li ()
      (strong () (a (:href (node-link)) (node-field 'title)))
      (ul ()
          (node-field 'child 'toc))))

(associate-template +qua-hub-section+ 'toc qua-hub-section-template-toc)

(deftemplate qua-hub-manual-operator-template-medium
  (div ()
       (h3 () (a (:id (node-anchor))
                 (node-field 'title) (node-field 'type-name)))
       (h4 () "Syntax")
       (em (:style "text-transform: lowercase")
           (strong () (node-field 'title)) " "
           (node-field 'syntax))
       (h4 () "Description")
       (div () (node-field 'content 'default))))

(associate-template +qua-hub-manual-operator+ 'default qua-hub-manual-operator-template-medium)

(deftemplate qua-hub-manual-operator-template-toc
  (li ()
      (strong (:style "text-transform: lowercase")
              (a (:href (node-link)) (node-field 'title)))
      " " (node-field 'syntax)))

(associate-template +qua-hub-manual-operator+ 'toc qua-hub-manual-operator-template-toc)

(deftemplate qua-hub-paragraph-template-medium
  (p () (node-field 'text)))

(associate-template +qua-hub-paragraph+ 'default qua-hub-paragraph-template-medium)

(deftemplate qua-hub-code-sample-template-medium
  (pre () (node-field 'text)))

(associate-template +qua-hub-code-sample+ 'default qua-hub-code-sample-template-medium)

(deftemplate qua-hub-template-large
  (div ()
       (h1 () (node-field 'title))
       (h2 () (tt () "(print \"Welcome to Qua!\")"))
       (div ()
            (node-field 'child 'medium))))

(associate-template +qua-hub-main-page+ 'large qua-hub-template-large)

(deftemplate qua-hub-page-template-large
  (div ()
       (h1 ()
           (a (:href "index.html") "Qua") " / " (node-field 'title))
       (div ()
            (node-field 'child 'default))))

(associate-template +qua-hub-page+ 'large qua-hub-page-template-large)

(defun page-link ()
  (a (:href (node-link)) (node-field 'title)))

(deftemplate qua-hub-page-template-medium
  (div ()
       (h3 () (page-link))
       (p () (node-field 'byline))))

(associate-template +qua-hub-page+ 'medium qua-hub-page-template-medium)

(deftemplate qua-hub-ref-template-inline
  (span (:class "ref-inline")
        "["
        (a (:href (node-field 'url))
           (node-field 'title))
        "]("
        (a (:href (node-link))
           "ref")
        ")"))

(associate-template +qua-hub-ref+ 'inline qua-hub-ref-template-inline)

(deftemplate qua-hub-ref-template-default
  (div (:class "ref")
       (a (:id (node-anchor)) (h3 () (node-field 'title)))
       (p () (node-field 'authors))
       (p () (a (:href (node-field 'url)) (node-field 'url)))))

(associate-template +qua-hub-ref+ 'default qua-hub-ref-template-default)
