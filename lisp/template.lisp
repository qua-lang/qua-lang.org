(defun main-template (target)
  (div ()
   (h1 () (attribute target 'title))
   (div ()
    (attribute target 'child #'medium-template))))

(defun page-template (target)
  (div ()
   (h1 () (a (:href "index.html") "Qua") " / " (attribute target 'title))
   (div ()
    (attribute target 'child #'medium-template))))

(defun medium-template (target)
  (div ()
   (h3 () (node-link target))
   (p () (attribute target 'byline))))

(defun manual-template (target)
  (div ()
       (center () (h1 () (a (:href "index.html") "Qua") " Language Manual"))
       (div ()
            (attribute target 'child #'section-template))))

(defun section-template (target)
  (div ()
   (h2 () (node-link target))
   (attribute target 'child #'item-template)))

(defun item-template (target)
  (h3 () (node-link target)))

