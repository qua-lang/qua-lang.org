(defconstant +special+ "Special")
(defconstant +function+ "Function")

(defnode index
  (:title "Qua: Ultralight Lisp for the Web")
  (:child 'alpha 'intro 'manual 'tools 'sample))

(defnode alpha
  (:title "Hackers wanted")
  (:byline "For alpha testing Qua, constant language changes, abysmal
  performance, undocumented code, long sessions of debugging, random
  crashes, fun and glorious hacks in case of success."))

(defnode intro
  (:title "Introduction to the Qua Project")
  (:byline "Start here if you are new to Qua."))

(defnode manual
  (:title "Qua Language Manual")
  (:byline "Reference for the Lisp dialect implemented by Qua.")
  (:child
   'sec-evaluation
   'sec-bindings
   'sec-places
   'sec-simple-control
   'sec-delimited-control))

(defnode sec-evaluation
  (:title "Evaluation")
  (:child 'op-vau
          'op-wrap
          'op-unwrap
          'op-lambda
          'op-eval
          'op-apply
          'op-funcall
          'op-quote))

(deffexpr gloref (foo) #ign
  foo)

(defnode op-vau
  (:title "VAU")
  (:type +special+)
  (:syntax "(vau operand-tree environment-parameter form*) => fexpr")
  (:description "Creates a new " (gloref "fexpr") " with the
  given " (gloref "operand tree") ", " (gloref "environment
  parameter") ", and body forms."))

(defnode op-wrap
  (:title "WRAP")
  (:type +function+)
  (:syntax "(wrap fexpr) => function"))

(defnode op-unwrap
  (:title "UNWRAP")
  (:type +function+)
  (:syntax "(unwrap function) => fexpr"))

(defnode op-lambda
  (:title "LAMBDA")
  (:type +special+)
  (:syntax "(lambda parameter-tree form*) => function"))

(defnode op-eval
  (:title "EVAL")
  (:type +function+)
  (:syntax "(eval form environment) => result"))

(defnode op-apply
  (:title "APPLY")
  (:type +function+)
  (:syntax "(apply function arguments) => result"))

(defnode op-funcall
  (:title "FUNCALL")
  (:type +function+)
  (:syntax "(funcall function argument*) => result"))

(defnode op-quote
  (:title "QUOTE")
  (:type +special+)
  (:syntax "(quote form) => form"))

(defnode sec-bindings
  (:title "Bindings and Environments")
  (:child 'op-make-environment
          'op-the-environment
          'op-def
          'op-defconstant))

(defnode op-def
  (:title "DEF")
  (:type +special+))

(defnode op-defconstant
  (:title "DEFCONSTANT")
  (:type +special+))

(defnode sec-places
  (:title "Places")
  (:child 'op-setq
          'op-setf
          'op-defsetf))

(defnode op-setq
  (:title "SETQ")
  (:type +special+))

(defnode op-setf
  (:title "SETF")
  (:type +special+))

(defnode op-defsetf
  (:title "DEFSETF")
  (:type +special+))

(defnode op-make-environment
  (:title "MAKE-ENVIRONMENT")
  (:type +special+))

(defnode op-the-environment
  (:title "THE-ENVIRONMENT")
  (:type +special+))

(defnode sec-simple-control
  (:title "Simple Control")
  (:child 'op-progn
          'op-loop))

(defnode op-progn
  (:title "PROGN")
  (:type +special+))

(defnode op-loop
  (:title "LOOP")
  (:type +special+))

(defnode sec-delimited-control
  (:title "Delimited Control")
  (:child 'op-push-prompt
          'op-take-subcont
          'op-push-subcont
          'op-push-prompt-subcont))

(defnode op-push-prompt
  (:title "PUSH-PROMPT")
  (:type +special+))

(defnode op-take-subcont
  (:title "TAKE-SUBCONT")
  (:type +special+))

(defnode op-push-subcont
  (:title "PUSH-SUBCONT")
  (:type +special+))

(defnode op-push-prompt-subcont
  (:title "PUSH-PROMPT-SUBCONT")
  (:type +special+))

(defnode tools
  (:title "Qua Tools Guide")
  (:byline "How to use Qua in HTML pages, web apps, and Node."))

(defnode sample
  (:title "Qua Demos and Sample Code")
  (:byline "Code already written in Qua, such as the generator for
  this website, for you to look at."))
