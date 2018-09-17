(deftemplate qua-hub-template-wrapper
  (html ()
        (head ()
              (meta (:charset "UTF-8"))
              (meta (:name "viewport" :content "width=device-width, initial-scale=1.0"))
              (title () (node-field 'title))
              (link (:rel "stylesheet" :type "text/css" :href "style/style.css")))
        (body ()
              (div (:class "navbar")
                   "("
                   (a (:href "index.html") "qua")
                   " '("
                   (a (:href "intro.html") "intro")
                   " "
                   (a (:href "manual.html") "manual")
                   " "
                   (a (:href "usage.html") "usage")
                   " "
                   (a (:href +qua-repl-url+) "repl")
                   " "
                   (a (:href +qua-repo-url+) "github")
                   "))")
              (call-template 'default))))

;;;; Main page template

(deftemplate qua-hub-main-page-template-default
  (div ()
       (div (:class "stats")
            (table ()
                   (tr ()
                       (th (:colspan "2" :class "stats-header")
                           "Stats for Qua 0.1"))
                   (tr ()
                       (th () "VM Lines of Code (per " (tt () "cloc") ")")
                       (td () "980"))
                   (tr ()
                       (th () "Size (Minified, Gzipped)")
                       (td () "15 KB"))
                   (tr ()
                       (th () "VM Startup Time (2015 Laptop)")
                       (td () "100 ms"))))
       (h1 () (node-field 'title))
       (div ()
            (node-field 'child 'medium))))

(associate-template +qua-hub-main-page+ 'default qua-hub-main-page-template-default)

;;;; Default page template

(deftemplate qua-hub-page-template-default
  (div ()
       (h1 ()
           (node-field 'title))
       (div ()
            (node-field 'child 'default))))

(associate-template +qua-hub-page+ 'default qua-hub-page-template-default)

;;;; Manual page template

(deftemplate qua-hub-manual-template-default
  (div ()
       (center ()
               (h1 () "Qua Manual")
               (strong () "Manuel J. Simoni"))
       (blockquote () (node-field 'abstract 'default))
       (h3 () "Table of Contents")
       (ul ()
           (node-field 'child 'manual-toc))
       (div ()
            (node-field 'child 'default))))

(associate-template +qua-hub-manual-page+ 'default qua-hub-manual-template-default)

;;;; Inline templates

(deftemplate qua-hub-inline-template
  (a (:href (node-link)) (node-field 'title)))

(associate-template +tcmplr-node+ 'inline qua-hub-inline-template)

(deftemplate qua-hub-inline-titled-template
  (a (:href (node-link)) (anchor-title)))

(associate-template +tcmplr-node+ 'inline-titled qua-hub-inline-titled-template)

(deftemplate qua-hub-concept-inline-template
  (a (:href (node-link)) (node-field 'title)))
(deftemplate qua-hub-concept-inline-titled-template
  (a (:href (node-link)) (anchor-title)))

(associate-template +qua-hub-manual-concept+ 'inline qua-hub-concept-inline-template)
(associate-template +qua-hub-manual-concept+ 'inline-titled qua-hub-concept-inline-titled-template)

(deftemplate qua-hub-op-inline-template
  (a (:href (node-link) :style "text-transform: lowercase") (strong () (node-field 'title))))

(associate-template +qua-hub-manual-class+ 'inline qua-hub-op-inline-template)
(associate-template +qua-hub-manual-constant+ 'inline qua-hub-op-inline-template)
(associate-template +qua-hub-manual-operator+ 'inline qua-hub-op-inline-template)

(deftemplate qua-hub-weblink-inline-template
  (a (:href (node-field 'url)) (node-field 'title)))

(associate-template +qua-hub-weblink+ 'inline qua-hub-weblink-inline-template)

;;;; Section templates

(deftemplate qua-hub-section-template-medium
  (div ()
       (a (:id (node-anchor)) (h2 () (node-field 'title)))
       (node-field 'content 'default)
       (node-field 'child 'default)))

(associate-template +qua-hub-section+ 'default qua-hub-section-template-medium)

(deftemplate qua-hub-section-template-manual-toc
  (li ()
      (strong () (a (:href (node-link)) (node-field 'title)))
      (ul ()
          (node-field 'child 'manual-toc))))

(associate-template +qua-hub-section+ 'manual-toc qua-hub-section-template-manual-toc)

;;;; Item templates

(deftemplate qua-hub-manual-operator-syntax-template
  (div ()
       (h4 () "Syntax:")
       (tt ()
           (strong (:style "text-transform: lowercase") (node-field 'title))
           " "
           (node-field 'syntax))))

(associate-template +qua-hub-manual-operator+ 'syntax qua-hub-manual-operator-syntax-template)

(deftemplate qua-hub-manual-constant-syntax-template
  (div ()
       (h4 () "Syntax:")
       (tt () (strong () (node-field 'syntax)))))

(associate-template +qua-hub-manual-constant+ 'syntax qua-hub-manual-constant-syntax-template)

(deftemplate qua-hub-manual-syntax-syntax-template
  (div ()
       (h4 () "Syntax:")
       (tt () (node-field 'syntax))))

(associate-template +qua-hub-manual-syntax+ 'syntax qua-hub-manual-syntax-syntax-template)

(deftemplate qua-hub-manual-default-syntax-template
  (div ()))

(associate-template +qua-hub-item+ 'syntax qua-hub-manual-default-syntax-template)

(deftemplate qua-hub-manual-operator-template-medium
  (div ()
       (h3 (:class "operator-title")
           (a (:id (node-anchor))
              (node-field 'title)
                 " "
                 (em () " (" (node-field 'type-name) ")")))
       (call-template 'syntax)
       (h4 () "Description:")
       (div () (node-field 'content 'default))
       (h4 () "Examples:")
       (pre () (node-field 'example 'default))
       (h4 () "Rationale:")
       (div () (node-field 'rationale 'default))))

(associate-template +qua-hub-manual-operator+ 'default qua-hub-manual-operator-template-medium)
(associate-template +qua-hub-manual-syntax+ 'default qua-hub-manual-operator-template-medium)
(associate-template +qua-hub-manual-constant+ 'default qua-hub-manual-operator-template-medium)
(associate-template +qua-hub-manual-class+ 'default qua-hub-manual-operator-template-medium)
(associate-template +qua-hub-manual-concept+ 'default qua-hub-manual-operator-template-medium)

(deftemplate qua-hub-item-template-manual-toc
  (li ()
      (a (:href (node-link))
         (strong () (node-field 'title)))
      (tt ()
          " " (node-field 'syntax) " ")
      (em ()
          "(" (node-field 'type-name) ")")))

(associate-template +qua-hub-item+ 'manual-toc qua-hub-item-template-manual-toc)

(deftemplate qua-hub-concept-template-manual-toc
  (li ()
      (a (:href (node-link))
         (strong () (em () (node-field 'title))))
      (tt () " ")
      (em ()
          "(" (node-field 'type-name) ")")))

(associate-template +qua-hub-manual-concept+ 'manual-toc qua-hub-concept-template-manual-toc)

(deftemplate qua-hub-paragraph-template-medium
  (p () (node-field 'text)))

(associate-template +qua-hub-paragraph+ 'default qua-hub-paragraph-template-medium)

(deftemplate qua-hub-code-sample-template-medium
  (pre () (node-field 'text)))

(associate-template +qua-hub-code-sample+ 'default qua-hub-code-sample-template-medium)

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
        "]"))

(associate-template +qua-hub-ref+ 'inline qua-hub-ref-template-inline)

(deftemplate qua-hub-ref-template-default
  (div (:class "ref")
       (a (:id (node-anchor)) (h3 () (node-field 'title)))
       (p () (node-field 'authors))
       (p () (a (:href (node-field 'url)) (node-field 'url)))))

(associate-template +qua-hub-ref+ 'default qua-hub-ref-template-default)
