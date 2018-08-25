(defnode (manual) +qua-hub-manual-page+
  (:title "Qua Lisp Manual")
  (:byline "Reference for the Lisp dialect implemented by Qua, a blend
  of Kernel, Common Lisp, and Scheme.")
  (:header (image (:src "../img/silly-parens.png") (:border 4)))
  (:abstract
   (paragraph "Qua Lisp, or just Qua for short, is a new dialect of
    Lisp, designed to be ultra-light and allow tiny implementations,
    yet offer the powerful metaprogramming facilities, control flow
    abstractions, and general no-nonsense approach that Lisp
    programmers know and love.  Qua is based on Kernel, Common Lisp,
    and Scheme.  From Kernel it takes its central computing
    workhorses, lexically-scoped fexprs and first-class environments.
    The surface syntax and core language look and feel a lot like
    Common Lisp, from which Qua inherits many operators.  The
    interface for control flow manipulation, delimited continuations,
    is the result of a long line of research pioneered in Scheme.")
   (paragraph "Qua Lisp is still unfinished.  Especially the areas of
    strings, numbers, objects, classes, generic functions, methods,
    sequences, streams, and the JS interface are still in flux and/or
    unspecified.  Nevertheless, it is already a practical language and
    the functionality documented in this manual is not expected to
    change in major ways."))
  (:child
   (hyper '(manual . sec-evaluation))
   (hyper '(manual . sec-environments))
   (hyper '(manual . sec-objects))
   (hyper '(manual . sec-generic-functions))
   (hyper '(manual . sec-places))
   (hyper '(manual . sec-numbers))
   (hyper '(manual . sec-symbols))
   (hyper '(manual . sec-lists))
   (hyper '(manual . sec-sequences))
   (hyper '(manual . sec-control))
   (hyper '(manual . sec-dynamic))
   (hyper '(manual . sec-conditions))
   (hyper '(manual . sec-continuations))
   (hyper '(manual . sec-js))))


(defnode (manual . sec-evaluation) +qua-hub-section+
  (:title "Evaluation")
  (:child
   (hyper '(manual . op-vau))
   (hyper '(manual . op-deffexpr))
   (hyper '(manual . op-wrap))
   (hyper '(manual . op-unwrap))
   (hyper '(manual . op-lambda))
   (hyper '(manual . op-defun))
   (hyper '(manual . op-function))
   (hyper '(manual . op-eval))
   (hyper '(manual . op-apply))
   (hyper '(manual . op-funcall))
   (hyper '(manual . op-quote))
   (hyper '(manual . op-macro))
   (hyper '(manual . op-defmacro))))

(defnode (manual . op-vau) +qua-hub-manual-operator+
  (:title "VAU")
  (:syntax "operand-tree environment-parameter form* => fexpr"))

(defnode (manual . op-deffexpr) +qua-hub-manual-operator+
  (:title "DEFFEXPR")
  (:syntax "name operand-tree environment-parameter form* => fexpr"))

(defnode (manual . op-wrap) +qua-hub-manual-operator+
  (:title "WRAP")
  (:syntax "fexpr => function"))

(defnode (manual . op-unwrap) +qua-hub-manual-operator+
  (:title "UNWRAP")
  (:syntax "function => fexpr"))

(defnode (manual . op-lambda) +qua-hub-manual-operator+
  (:title "LAMBDA")
  (:syntax "parameter-tree form* => function"))

(defnode (manual . op-defun) +qua-hub-manual-operator+
  (:title "DEFUN")
  (:syntax "name parameter-tree form* => function"))

(defnode (manual . op-function) +qua-hub-manual-operator+
  (:title "FUNCTION")
  (:syntax "name => function"))

(defnode (manual . op-eval) +qua-hub-manual-operator+
  (:title "EVAL")
  (:syntax "form environment => result"))

(defnode (manual . op-apply) +qua-hub-manual-operator+
  (:title "APPLY")
  (:syntax "function arguments => result"))

(defnode (manual . op-funcall) +qua-hub-manual-operator+
  (:title "FUNCALL")
  (:syntax "function argument* => result"))

(defnode (manual . op-quote) +qua-hub-manual-operator+
  (:title "QUOTE")
  (:syntax "form => form"))

(defnode (manual . op-macro) +qua-hub-manual-operator+
  (:title "MACRO")
  (:syntax "operand-tree form* => macro"))

(defnode (manual . op-defmacro) +qua-hub-manual-operator+
  (:title "DEFMACRO")
  (:syntax "name operand-tree form* => macro"))


(defnode (manual . sec-environments) +qua-hub-section+
  (:title "Environments")
  (:child
   (hyper '(manual . op-def))
   (hyper '(manual . op-defconstant))
   (hyper '(manual . op-setq))
   (hyper '(manual . op-let))
   (hyper '(manual . op-let-star))
   (hyper '(manual . op-flet))
   (hyper '(manual . op-labels))
   (hyper '(manual . op-make-environment))
   (hyper '(manual . op-the-environment))))

(defnode (manual . op-def) +qua-hub-manual-operator+
  (:title "DEF")
  (:syntax "definiend value => result"))

(defnode (manual . op-defconstant) +qua-hub-manual-operator+
  (:title "DEFCONSTANT")
  (:syntax "definiend value => result"))

(defnode (manual . op-setq) +qua-hub-manual-operator+
  (:title "SETQ")
  (:syntax "definiend value => result"))

(defnode (manual . op-let) +qua-hub-manual-operator+
  (:title "LET")
  (:syntax "((var value)*) body* => result"))

(defnode (manual . op-let-star) +qua-hub-manual-operator+
  (:title "LET*")
  (:syntax "((var value)*) body* => result"))

(defnode (manual . op-flet) +qua-hub-manual-operator+
  (:title "FLET")
  (:syntax "((function-name lambda-list form*)*) body* => result"))

(defnode (manual . op-labels) +qua-hub-manual-operator+
  (:title "LABELS")
  (:syntax "((function-name lambda-list form*)*) body* => result"))

(defnode (manual . op-make-environment) +qua-hub-manual-operator+
  (:title "MAKE-ENVIRONMENT")
  (:syntax "[parent] => environment"))

(defnode (manual . op-the-environment) +qua-hub-manual-operator+
  (:title "THE-ENVIRONMENT")
  (:syntax "=> current-environment"))


(defnode (manual . sec-objects) +qua-hub-section+
  (:title "Objects")
  (:child
   (hyper '(manual . op-defstruct))
   (hyper '(manual . op-make-instance))
   (hyper '(manual . op-class-of))
   (hyper '(manual . op-find-class))
   (hyper '(manual . op-class))
   (hyper '(manual . op-slot-value))
   (hyper '(manual . op-set-slot-value))
   (hyper '(manual . op-slot-boundp))
   (hyper '(manual . op-eq))
   (hyper '(manual . op-eql))
   (hyper '(manual . op-typep))
   (hyper '(manual . op-typecase))
   (hyper '(manual . op-the))))

(defnode (manual . op-defstruct) +qua-hub-manual-operator+
  (:title "DEFSTRUCT")
  (:syntax "name slot-name* => void"))

(defnode (manual . op-make-instance) +qua-hub-manual-operator+
  (:title "MAKE-INSTANCE")
  (:syntax "class . initargs => instance"))

(defnode (manual . op-class-of) +qua-hub-manual-operator+
  (:title "CLASS-OF")
  (:syntax "instance => class"))

(defnode (manual . op-find-class) +qua-hub-manual-operator+
  (:title "FIND-CLASS")
  (:syntax "symbol => class"))

(defnode (manual . op-class) +qua-hub-manual-operator+
  (:title "CLASS")
  (:syntax "name => class"))

(defnode (manual . op-slot-value) +qua-hub-manual-operator+
  (:title "SLOT-VALUE")
  (:syntax "instance slot-name => value"))

(defnode (manual . op-set-slot-value) +qua-hub-manual-operator+
  (:title "SET-SLOT-VALUE")
  (:syntax "instance slot-name slot-value => value"))

(defnode (manual . op-slot-boundp) +qua-hub-manual-operator+
  (:title "SLOT-BOUND?")
  (:syntax "instance slot-name => boolean"))

(defnode (manual . op-eq) +qua-hub-manual-operator+
  (:title "EQ")
  (:syntax "value1 value2 => result"))

(defnode (manual . op-eql) +qua-hub-manual-operator+
  (:title "EQL")
  (:syntax "value1 value2 => result"))

(defnode (manual . op-typep) +qua-hub-manual-operator+
  (:title "TYPE?")
  (:syntax "object type-spec => result"))

(defnode (manual . op-typecase) +qua-hub-manual-operator+
  (:title "TYPECASE")
  (:syntax "object (type-spec form*)* => result"))

(defnode (manual . op-the) +qua-hub-manual-operator+
  (:title "THE")
  (:syntax "object type => result"))


(defnode (manual . sec-generic-functions) +qua-hub-section+
  (:title "Generic Functions")
  (:child
   (hyper '(manual . op-defgeneric))
   (hyper '(manual . op-defmethod))))

(defnode (manual . op-defgeneric) +qua-hub-manual-operator+
  (:title "DEFGENERIC")
  (:syntax "name (param*) => void"))

(defnode (manual . op-defmethod) +qua-hub-manual-operator+
  (:title "DEFMETHOD")
  (:syntax "name ((self class) param*) form* => void"))


(defnode (manual . sec-places) +qua-hub-section+
  (:title "Places")
  (:child
   (hyper '(manual . op-setf))
   (hyper '(manual . op-defsetf))
   (hyper '(manual . op-setter))
   (hyper '(manual . op-incf))
   (hyper '(manual . op-decf))))

(defnode (manual . op-setf) +qua-hub-manual-operator+
  (:title "SETF")
  (:syntax "place value => result"))

(defnode (manual . op-defsetf) +qua-hub-manual-operator+
  (:title "DEFSETF")
  (:syntax "accessor-fn update-fn => void"))

(defnode (manual . op-setter) +qua-hub-manual-operator+
  (:title "SETTER")
  (:syntax "function => setter-function"))

(defnode (manual . op-incf) +qua-hub-manual-operator+
  (:title "INCF")
  (:syntax "place [increment] => result"))

(defnode (manual . op-decf) +qua-hub-manual-operator+
  (:title "DECF")
  (:syntax "place [decrement] => result"))


(defnode (manual . sec-numbers) +qua-hub-section+
  (:title "Numbers")
  (:child
   (hyper '(manual . op-lt))
   (hyper '(manual . op-lte))
   (hyper '(manual . op-gt))
   (hyper '(manual . op-gte))
   (hyper '(manual . op-add))
   (hyper '(manual . op-sub))
   (hyper '(manual . op-mul))
   (hyper '(manual . op-div))))

(defnode (manual . op-lt) +qua-hub-manual-operator+
  (:title "<")
  (:syntax "number+ => result"))

(defnode (manual . op-lte) +qua-hub-manual-operator+
  (:title "<=")
  (:syntax "number+ => result"))

(defnode (manual . op-gt) +qua-hub-manual-operator+
  (:title ">")
  (:syntax "number+ => result"))

(defnode (manual . op-gte) +qua-hub-manual-operator+
  (:title ">=")
  (:syntax "number+ => result"))

(defnode (manual . op-add) +qua-hub-manual-operator+
  (:title "+")
  (:syntax "number* => result"))

(defnode (manual . op-sub) +qua-hub-manual-operator+
  (:title "-")
  (:syntax "number+ => result"))

(defnode (manual . op-mul) +qua-hub-manual-operator+
  (:title "*")
  (:syntax "number* => result"))

(defnode (manual . op-div) +qua-hub-manual-operator+
  (:title "/")
  (:syntax "number+ => result"))


(defnode (manual . sec-symbols) +qua-hub-section+
  (:title "Symbols")
  (:child
   (hyper '(manual . op-make-symbol))
   (hyper '(manual . op-symbol-name))
   (hyper '(manual . op-function-symbol))
   (hyper '(manual . op-type-symbol))))

(defnode (manual . op-make-symbol) +qua-hub-manual-operator+
  (:title "MAKE-SYMBOL")
  (:syntax "string => symbol"))

(defnode (manual . op-symbol-name) +qua-hub-manual-operator+
  (:title "SYMBOL-NAME")
  (:syntax "symbol => string"))

(defnode (manual . op-function-symbol) +qua-hub-manual-operator+
  (:title "FUNCTION-SYMBOL")
  (:syntax "symbol => function-symbol"))

(defnode (manual . op-type-symbol) +qua-hub-manual-operator+
  (:title "TYPE-SYMBOL")
  (:syntax "symbol => type-symbol"))


(defnode (manual . sec-lists) +qua-hub-section+
  (:title "Lists")
  (:child
   (hyper '(manual . op-cons))
   (hyper '(manual . op-car))
   (hyper '(manual . op-cdr))
   (hyper '(manual . op-caar))
   (hyper '(manual . op-cadr))
   (hyper '(manual . op-cdar))
   (hyper '(manual . op-cddr))
   (hyper '(manual . op-list))
   (hyper '(manual . op-list-star))
   (hyper '(manual . op-reverse-list))))

(defnode (manual . op-cons) +qua-hub-manual-operator+
  (:title "CONS")
  (:syntax "car cdr => cons"))

(defnode (manual . op-car) +qua-hub-manual-operator+
  (:title "CAR")
  (:syntax "x => result"))

(defnode (manual . op-cdr) +qua-hub-manual-operator+
  (:title "CDR")
  (:syntax "x => result"))

(defnode (manual . op-caar) +qua-hub-manual-operator+
  (:title "CAAR")
  (:syntax "x => result"))

(defnode (manual . op-cadr) +qua-hub-manual-operator+
  (:title "CADR")
  (:syntax "x => result"))

(defnode (manual . op-cdar) +qua-hub-manual-operator+
  (:title "CDAR")
  (:syntax "x => result"))

(defnode (manual . op-cddr) +qua-hub-manual-operator+
  (:title "CDDR")
  (:syntax "x => result"))

(defnode (manual . op-list) +qua-hub-manual-operator+
  (:title "LIST")
  (:syntax "value* => list"))

(defnode (manual . op-list-star) +qua-hub-manual-operator+
  (:title "LIST*")
  (:syntax "value* list => list"))

(defnode (manual . op-reverse-list) +qua-hub-manual-operator+
  (:title "REVERSE-LIST")
  (:syntax "list => list"))


(defnode (manual . sec-sequences) +qua-hub-section+
  (:title "Sequences")
  (:child
   (hyper '(manual . op-map))
   (hyper '(manual . op-for-each))
   (hyper '(manual . op-subseq))))

(defnode (manual . op-map) +qua-hub-manual-operator+
  (:title "MAP")
  (:syntax "function sequence => sequence"))

(defnode (manual . op-for-each) +qua-hub-manual-operator+
  (:title "FOR-EACH")
  (:syntax "function sequence => void"))

(defnode (manual . op-subseq) +qua-hub-manual-operator+
  (:title "SUBSEQ")
  (:syntax "sequence start [end] => sequence"))


(defnode (manual . sec-control) +qua-hub-section+
  (:title "Control Flow")
  (:child
   (hyper '(manual . op-progn))
   (hyper '(manual . op-prog1))
   (hyper '(manual . op-prog2))
   (hyper '(manual . op-if))
   (hyper '(manual . op-when))
   (hyper '(manual . op-unless))
   (hyper '(manual . op-cond))
   (hyper '(manual . op-case))
   (hyper '(manual . op-and))
   (hyper '(manual . op-or))
   (hyper '(manual . op-not))
   (hyper '(manual . op-block))
   (hyper '(manual . op-return-from))
   (hyper '(manual . op-unwind-protect))
   (hyper '(manual . op-loop))
   (hyper '(manual . op-while))
   (hyper '(manual . op-dotimes))))

(defnode (manual . op-progn) +qua-hub-manual-operator+
  (:title "PROGN")
  (:syntax "form* => result"))

(defnode (manual . op-prog1) +qua-hub-manual-operator+
  (:title "PROG1")
  (:syntax "form* => result"))

(defnode (manual . op-prog2) +qua-hub-manual-operator+
  (:title "PROG2")
  (:syntax "form form* => result"))

(defnode (manual . op-if) +qua-hub-manual-operator+
  (:title "IF")
  (:syntax "test consequent alternative => result"))

(defnode (manual . op-when) +qua-hub-manual-operator+
  (:title "WHEN")
  (:syntax "test form* => result"))

(defnode (manual . op-unless) +qua-hub-manual-operator+
  (:title "UNLESS")
  (:syntax "test form* => result"))

(defnode (manual . op-cond) +qua-hub-manual-operator+
  (:title "COND")
  (:syntax "(test form*)* => result"))

(defnode (manual . op-case) +qua-hub-manual-operator+
  (:title "CASE")
  (:syntax "keyform (key form*)* => result"))

(defnode (manual . op-and) +qua-hub-manual-operator+
  (:title "AND")
  (:syntax "form* => result"))

(defnode (manual . op-or) +qua-hub-manual-operator+
  (:title "OR")
  (:syntax "form* => result"))

(defnode (manual . op-not) +qua-hub-manual-operator+
  (:title "NOT")
  (:syntax "value => result"))

(defnode (manual . op-block) +qua-hub-manual-operator+
  (:title "BLOCK")
  (:syntax "tag form* => result"))

(defnode (manual . op-return-from) +qua-hub-manual-operator+
  (:title "RETURN-FROM")
  (:syntax "tag [value] => |"))

(defnode (manual . op-unwind-protect) +qua-hub-manual-operator+
  (:title "UNWIND-PROTECT")
  (:syntax "protected-form cleanup-form* => result"))

(defnode (manual . op-loop) +qua-hub-manual-operator+
  (:title "LOOP")
  (:syntax "form* => |"))

(defnode (manual . op-while) +qua-hub-manual-operator+
  (:title "WHILE")
  (:syntax "test form* => |"))

(defnode (manual . op-dotimes) +qua-hub-manual-operator+
  (:title "DOTIMES")
  (:syntax "(var count-form [result-form]) form* => result"))


(defnode (manual . sec-continuations) +qua-hub-section+
  (:title "Delimited Continuations")
  (:child
   (hyper '(manual . op-push-prompt))
   (hyper '(manual . op-take-subcont))
   (hyper '(manual . op-push-subcont))
   (hyper '(manual . op-push-prompt-subcont))
   (hyper '(manual . op-push-default-prompt))
   (hyper '(manual . op-take-default-subcont))
   (hyper '(manual . op-push-default-subcont))))

(defnode (manual . op-push-prompt) +qua-hub-manual-operator+
  (:title "PUSH-PROMPT")
  (:syntax "prompt form* => result"))

(defnode (manual . op-take-subcont) +qua-hub-manual-operator+
  (:title "TAKE-SUBCONT")
  (:syntax "prompt continuation form* => result"))

(defnode (manual . op-push-subcont) +qua-hub-manual-operator+
  (:title "PUSH-SUBCONT")
  (:syntax "continuation form* => result"))

(defnode (manual . op-push-prompt-subcont) +qua-hub-manual-operator+
  (:title "PUSH-PROMPT-SUBCONT")
  (:syntax "prompt continuation form* => result"))

(defnode (manual . op-push-default-prompt) +qua-hub-manual-operator+
  (:title "PUSH-DEFAULT-PROMPT")
  (:syntax "form* => result"))

(defnode (manual . op-take-default-subcont) +qua-hub-manual-operator+
  (:title "TAKE-DEFAULT-SUBCONT")
  (:syntax "continuation form* => result"))

(defnode (manual . op-push-default-subcont) +qua-hub-manual-operator+
  (:title "PUSH-DEFAULT-SUBCONT")
  (:syntax "continuation form* => result"))


(defnode (manual . sec-dynamic) +qua-hub-section+
  (:title "Dynamic Variables")
  (:child
   (hyper '(manual . op-defdynamic))
   (hyper '(manual . op-dynamic-bind))
   (hyper '(manual . op-dynamic))))

(defnode (manual . op-defdynamic) +qua-hub-manual-operator+
  (:title "DEFDYNAMIC")
  (:syntax "name [value] => void"))

(defnode (manual . op-dynamic-bind) +qua-hub-manual-operator+
  (:title "DYNAMIC-BIND")
  (:syntax "((dynamic-var value)*) form* => result"))

(defnode (manual . op-dynamic) +qua-hub-manual-operator+
  (:title "DYNAMIC")
  (:syntax "dynamic-var => value"))

(defnode (manual . op-progv) +qua-hub-manual-operator+
  (:title "PROGV")
  (:syntax "dynamic-var value function => result"))


(defnode (manual . sec-conditions) +qua-hub-section+
  (:title "Conditions and Restarts")
  (:child
   (hyper '(manual . op-handler-bind))
   (hyper '(manual . op-signal))
   (hyper '(manual . op-warn))
   (hyper '(manual . op-error))
   (hyper '(manual . op-restart-bind))
   (hyper '(manual . op-find-restart))
   (hyper '(manual . op-compute-restarts))
   (hyper '(manual . op-invoke-restart))
   (hyper '(manual . op-invoke-restart-interactively))
   (hyper '(manual . op-invoke-debugger))))

(defnode (manual . op-handler-bind) +qua-hub-manual-operator+
  (:title "HANDLER-BIND")
  (:syntax "((condition-type handler-function)*) form* => value"))

(defnode (manual . op-signal) +qua-hub-manual-operator+
  (:title "SIGNAL")
  (:syntax "condition => value"))

(defnode (manual . op-warn) +qua-hub-manual-operator+
  (:title "WARN")
  (:syntax "condition => value"))

(defnode (manual . op-error) +qua-hub-manual-operator+
  (:title "ERROR")
  (:syntax "condition => |"))

(defnode (manual . op-restart-bind) +qua-hub-manual-operator+
  (:title "RESTART-BIND")
  (:syntax "((restart-name function key-val-pair*)*) form* => result"))

(defnode (manual . op-invoke-restart) +qua-hub-manual-operator+
  (:title "INVOKE-RESTART")
  (:syntax "restart-designator argument* => result"))

(defnode (manual . op-invoke-restart-interactively) +qua-hub-manual-operator+
  (:title "INVOKE-RESTART-INTERACTIVELY")
  (:syntax "restart-designator => result"))

(defnode (manual . op-invoke-debugger) +qua-hub-manual-operator+
  (:title "INVOKE-DEBUGGER")
  (:syntax "condition => |"))

(defnode (manual . op-find-restart) +qua-hub-manual-operator+
  (:title "FIND-RESTART")
  (:syntax "restart-designator [associated-condition] => restart"))

(defnode (manual . op-compute-restarts) +qua-hub-manual-operator+
  (:title "COMPUTE-RESTARTS")
  (:syntax "[associated-condition] => restarts"))


(defnode (manual . sec-js) +qua-hub-section+
  (:title "JavaScript Interface")
  (:child
   (hyper '(manual . op-js-global))
   (hyper '(manual . op-js-new))
   (hyper '(manual . op-js-object))
   (hyper '(manual . op-js-array))
   (hyper '(manual . op-js-function))
   (hyper '(manual . op-js-lambda))
   (hyper '(manual . op-js-get))
   (hyper '(manual . op-js-set))
   (hyper '(manual . op-list-to-js-array))
   (hyper '(manual . op-plist-to-js-object))
   (hyper '(manual . op-log))))

(defnode (manual . op-js-global) +qua-hub-manual-operator+
  (:title "JS-GLOBAL")
  (:syntax "name => value"))

(defnode (manual . op-js-new) +qua-hub-manual-operator+
  (:title "JS-NEW")
  (:syntax "constructor argument* => object"))

(defnode (manual . op-js-get) +qua-hub-manual-operator+
  (:title "JS-GET")
  (:syntax "object key => value"))

(defnode (manual . op-js-set) +qua-hub-manual-operator+
  (:title "JS-SET")
  (:syntax "object key value => value"))

(defnode (manual . op-js-object) +qua-hub-manual-operator+
  (:title "JS-OBJECT")
  (:syntax ". initargs => object"))

(defnode (manual . op-js-array) +qua-hub-manual-operator+
  (:title "JS-ARRAY")
  (:syntax "value* => array"))

(defnode (manual . op-list-to-js-array) +qua-hub-manual-operator+
  (:title "LIST-TO-JS-ARRAY")
  (:syntax "list => array"))

(defnode (manual . op-plist-to-js-object) +qua-hub-manual-operator+
  (:title "PLIST-TO-JS-OBJECT")
  (:syntax "plist => object"))

(defnode (manual . op-js-function) +qua-hub-manual-operator+
  (:title "JS-FUNCTION")
  (:syntax "function => js-function"))

(defnode (manual . op-js-lambda) +qua-hub-manual-operator+
  (:title "JS-LAMBDA")
  (:syntax "lambda-list *form => js-function"))

(defnode (manual . op-log) +qua-hub-manual-operator+
  (:title "LOG")
  (:syntax "value* => result"))
