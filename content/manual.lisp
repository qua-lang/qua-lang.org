(defnode qua-hub-manual +qua-hub-manual-page+
  (:title "Qua Lisp Manual")
  (:url "manual.html")
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
