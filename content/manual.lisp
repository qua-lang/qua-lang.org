(defnode (manual) +qua-hub-manual-page+
  (:title "Qua Lisp Manual")
  (:url "manual.html")
  (:byline "Reference for the Lisp dialect implemented by Qua.")
  (:child
   (hyper '(manual . sec-evaluation))))

(defnode (manual . sec-evaluation) +qua-hub-section+
  (:title "Evaluation")
  (:child
   (hyper '(manual . op-vau))
   (hyper '(manual . op-wrap))
   (hyper '(manual . op-unwrap))
   (hyper '(manual . op-lambda))
   (hyper '(manual . op-eval))
   (hyper '(manual . op-apply))
   (hyper '(manual . op-funcall))
   (hyper '(manual . op-quote))))

(defnode (manual . op-vau) +qua-hub-manual-operator+
  (:title "VAU")
  (:syntax "vau operand-tree environment-parameter form* => fexpr"))

(defnode (manual . op-wrap) +qua-hub-manual-operator+
  (:title "WRAP")
  (:syntax "wrap fexpr => function"))

(defnode (manual . op-unwrap) +qua-hub-manual-operator+
  (:title "UNWRAP")
  (:syntax "unwrap function => fexpr"))

(defnode (manual . op-lambda) +qua-hub-manual-operator+
  (:title "LAMBDA")
  (:syntax "lambda parameter-tree form* => function"))

(defnode (manual . op-eval) +qua-hub-manual-operator+
  (:title "EVAL")
  (:syntax "eval form environment => result"))

(defnode (manual . op-apply) +qua-hub-manual-operator+
  (:title "APPLY")
  (:syntax "apply function arguments => result"))

(defnode (manual . op-funcall) +qua-hub-manual-operator+
  (:title "FUNCALL")
  (:syntax "funcall function argument* => result"))

(defnode (manual . op-quote) +qua-hub-manual-operator+
  (:title "QUOTE")
  (:syntax "quote form => form"))
