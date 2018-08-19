(define-node-type +qua-hub-page+)
(define-node-type +qua-hub-section+)
(define-node-type +qua-hub-item+)
(define-node-type +qua-hub-paragraph+)
(define-node-type +qua-hub-main-page+ +qua-hub-page+)
(define-node-type +qua-hub-manual-page+ +qua-hub-page+)
(define-node-type +qua-hub-manual-operator+ +qua-hub-item+)

(defconstant +qua-hub-lisp-name+ "Chronic Lisp")

(defnode qua-hub-index +qua-hub-main-page+
  (:title "Qua: Ultralight Lisp for the Web")
  (:child
   (hyper 'qua-hub-alpha)
   (hyper 'qua-hub-manual)))

(defnode qua-hub-alpha +qua-hub-page+
  (:title "Hackers wanted")
  (:page-name "alpha.html")
  (:byline "For alpha testing Qua, constant language changes, abysmal
  performance, undocumented code, long sessions of debugging, random
  crashes, fun and glorious hacks in case of success."))

(defnode qua-hub-intro +qua-hub-page+
  (:title "Introduction to the Qua Project")
  (:byline "Start here if you are new to Qua."))

(defnode qua-hub-manual +qua-hub-manual-page+
  (:title "Qua Lisp Manual")
  (:page-name "manual.html")
  (:byline "Reference for the Lisp dialect implemented by Qua.")
  (:child
   (hyper 'qua-hub-sec-evaluation)))

(defnode qua-hub-sec-evaluation +qua-hub-section+
  (:title "Evaluation")
  (:child
   (hyper 'op-vau)
   (hyper 'op-wrap)
   (hyper 'op-unwrap)
   (hyper 'op-lambda)
   (hyper 'op-eval)
   (hyper 'op-apply)
   (hyper 'op-funcall)
   (hyper 'op-quote)))

(defnode op-vau +qua-hub-manual-operator+
  (:title "VAU")
  (:syntax "vau operand-tree environment-parameter form* => fexpr"))

(defnode op-wrap +qua-hub-manual-operator+
  (:title "WRAP")
  (:syntax "wrap fexpr => function"))

(defnode op-unwrap +qua-hub-manual-operator+
  (:title "UNWRAP")
  (:syntax "unwrap function => fexpr"))

(defnode op-lambda +qua-hub-manual-operator+
  (:title "LAMBDA")
  (:syntax "lambda parameter-tree form* => function"))

(defnode op-eval +qua-hub-manual-operator+
  (:title "EVAL")
  (:syntax "eval form environment => result"))

(defnode op-apply +qua-hub-manual-operator+
  (:title "APPLY")
  (:syntax "apply function arguments => result"))

(defnode op-funcall +qua-hub-manual-operator+
  (:title "FUNCALL")
  (:syntax "funcall function argument* => result"))

(defnode op-quote +qua-hub-manual-operator+
  (:title "QUOTE")
  (:syntax "quote form => form"))
