(define-manual (manual)
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
   (hyper '(manual . sec-syntax))
   (hyper '(manual . sec-evaluation))
   (hyper '(manual . sec-environments))
   (hyper '(manual . sec-objects))
   (hyper '(manual . sec-generic-functions))
   (hyper '(manual . sec-places))
   (hyper '(manual . sec-booleans))
   (hyper '(manual . sec-numbers))
   (hyper '(manual . sec-strings))
   (hyper '(manual . sec-symbols))
   (hyper '(manual . sec-lists))
   (hyper '(manual . sec-sequences))
   (hyper '(manual . sec-control))
   (hyper '(manual . sec-dynamic))
   (hyper '(manual . sec-conditions))
   (hyper '(manual . sec-continuations))
   (hyper '(manual . sec-js))))


(define-manual-section (manual . sec-syntax)
  (:title "Syntax")
  (:child
   (hyper '(manual . stx-string))
   (hyper '(manual . stx-number))
   (hyper '(manual . stx-constant))
   (hyper '(manual . stx-symbol))
   (hyper '(manual . stx-function))
   (hyper '(manual . stx-keyword))
   (hyper '(manual . stx-list))
   (hyper '(manual . stx-dotted-list))
   (hyper '(manual . stx-quote))
   (hyper '(manual . stx-dynamic-variable))
   (hyper '(manual . stx-constant-variable))
   (hyper '(manual . stx-global-variable))
   (hyper '(manual . stx-js-global))
   (hyper '(manual . stx-js-property))
   (hyper '(manual . stx-js-method))))

(define-manual-syntax (manual . stx-string)
  (:title "String Syntax")
  (:syntax "\"characters\" \\\" \\t \\r \\n \\\\")
  (:content
   (paragraph
    "Qua string syntax follows " (hyper '(ref . json))
    " but only supports a subset of it at the moment."))
  (:example
   "\"foo \\\" bar\" => \"foo \\\" bar\"")
  (:rationale
   (paragraph
    "Given the close integration between Qua and JavaScript it seems
    to be a good idea to follow JavaScript's string syntax, and not
    more traditional Lisp syntax, so that programmers don't need to
    keep two syntaxes in their heads.")))

(define-manual-syntax (manual . stx-number)
  (:title "Number Syntax")
  (:syntax "[+|-]digits[.digits]")
  (:content
   (paragraph
    "Qua number syntax follows " (hyper '(ref . json))
    " but only supports a subset of it at the moment."))
  (:example
   "-12.34 => -12.34")
  (:rationale
   (paragraph
    "See rationale for " (hyper '(manual . stx-string)) ".")))

(define-manual-syntax (manual . stx-constant)
  (:title "Constant Syntax")
  (:syntax "#constant")
  (:content
   (paragraph
    "Built-in constants, such as " (hyper '(manual . const-t)) "
    or " (hyper '(manual . const-void)) " get a special syntax
    distinct from symbols."))
  (:example
   "#t => #t
#void => #void")
  (:rationale
   (paragraph
    "Avoiding to pollute the variable namespace with identifiers for constants,
    which should be short, seems to be a good idea.")))

(define-manual-syntax (manual . stx-symbol)
  (:title "Variable Symbol Syntax")
  (:syntax "symbol-name")
  (:content
   (paragraph
    "Variable symbols allow many characters except space (exact list
    to be determined) and should allow all alphanumeric Unicode
    characters at some point, but are currently restricted to
    ASCII."))
  (:example
   "foo-bar*"))

(define-manual-syntax (manual . stx-keyword)
  (:title "Keyword Symbol Syntax")
  (:syntax ":keyword-name")
  (:content
   (paragraph
    "Keyword symbols follow " (hyper '(manual . stx-symbol)) " as to
    the content of the symbol name."))
  (:example
   ":my-keyword => :my-keyword"))

(define-manual-syntax (manual . stx-function)
  (:title "Function Symbol Syntax")
  (:syntax "#'function-name")
  (:content
   (paragraph
    "Function symbols follow " (hyper '(manual . stx-symbol)) " as to
    the content of the symbol name."))
  (:example
   "#'+
#'quuxify"))

(define-manual-syntax (manual . stx-dynamic-variable)
  (:title "Dynamic Variable Syntax")
  (:syntax "*dynamic-name*")
  (:content
   (paragraph
    "Dynamic variable names are wrapped in ``*'' by convention."))
  (:example
   "*standard-output*")
  (:rationale
   (paragraph "Using special prefixes and suffixes prevents having to
   use prefixes like ``current-'' for dynamic variable names to
   distinguish them from local variables.")))

(define-manual-syntax (manual . stx-constant-variable)
  (:title "Constant Variable Syntax")
  (:syntax "+constant-name+")
  (:content
   (paragraph
    "Constant variable names are wrapped in ``+'' by convention.  This
    should be used for global variables that are never reassigned, and
    that are bound to objects whose contents are immutable."))
  (:example
   "(defconstant +my-constant+ 23)"))

(define-manual-syntax (manual . stx-global-variable)
  (:title "Global Variable Syntax")
  (:syntax "-global-name-")
  (:content
   (paragraph
    "Global variable names are wrapped in ``-'' by convention.  This
    should be used for global variables that are reassigned, like a
    global counter, or that are bound to objects whose contents are
    mutated, like a global hash table."))
  (:example "(def -my-global-hash-table- (js-object))")
  (:rationale
   (paragraph "It seems like a good idea to syntactically distinguish
   global variables, analogous to dynamic variables.")))

(define-manual-syntax (manual . stx-list)
  (:title "List Syntax")
  (:syntax "( element* )")
  (:content
   (paragraph
    "The usual syntax for lists."))
  (:example "(a big (nested (list)))

() === #nil"))

(define-manual-syntax (manual . stx-dotted-list)
  (:title "Dotted List Syntax")
  (:syntax "( element+ . element )")
  (:content
   (paragraph
    "The usual syntax for specifying the last element of a list
    explicitly."))
  (:example "(a dotted . list)

(1 2) === (1 . (2)) === (1 . (2 . #nil))"))

(define-manual-syntax (manual . stx-quote)
  (:title "Quotation Syntax")
  (:syntax "'form")
  (:content
   (paragraph
    "The usual syntax for preventing evaluation of a form, syntactic
    sugar for " (hyper '(manual . op-quote)) "."))
  (:example "'foo => foo
'#'foo => #'foo
'12 => 12
''foo => 'foo
'(+ 3 4) => (+ 3 4)"))

(define-manual-syntax (manual . stx-js-global)
  (:title "JS Global Variable Syntax")
  (:syntax "$variable")
  (:content
   (paragraph
    "Syntax for accessing a JavaScript global variable or function.
    Can be used with " (hyper '(manual . op-setf)) " to update the
    variable.  This is syntactic sugar for " (hyper '(manual
    . op-js-global)) "."))
  (:example
   ";; Used to access global variables ...
$window => #[js-object]
;; ... and call global JS functions:
($alert \"Hello world!\") => #void

(setf $SOME_JS_VARIABLE 12) => 12")
  (:rationale
   (paragraph "Qua needs convenient access to JS globals, so it seems
   to make sense to spend a special sigil for that purpose.")))

(define-manual-syntax (manual . stx-js-property)
  (:title "JS Property Syntax")
  (:syntax ".property")
  (:content
   (paragraph
    "Syntax for accessing a JavaScript property.  Can be used
    with " (hyper '(manual . op-setf)) " to update the property.  This
    is syntactic sugar for " (hyper '(manual . op-js-getter)) "."))
  (:example
   "(.title $document) => \"Qua Lisp Manual\"

(setf (.title $document) \"Qua rocks\")

;; Is actually shorthand for a function, so can be mapped across lists
;; of objects, etc:
(map .x (list (js-object :x 1 :y 2) (js-object :x 3 :y 4)))
=> (1 3)")
  (:rationale
   (paragraph "JS properties are accessed frequently so it makes sense
   to spend a sigil for that purpose.")))

(define-manual-syntax (manual . stx-js-method)
  (:title "JS Method Syntax")
  (:syntax "@method"))


(define-manual-section (manual . sec-evaluation)
  (:title "Evaluation")
  (:child
   (hyper '(manual . class-fexpr))
   (hyper '(manual . op-vau))
   (hyper '(manual . op-deffexpr))
   (hyper '(manual . class-function))
   (hyper '(manual . op-wrap))
   (hyper '(manual . op-unwrap))
   (hyper '(manual . op-lambda))
   (hyper '(manual . op-defun))
   (hyper '(manual . op-function))
   (hyper '(manual . op-eval))
   (hyper '(manual . op-apply))
   (hyper '(manual . op-funcall))
   (hyper '(manual . op-quote))
   (hyper '(manual . class-macro))
   (hyper '(manual . op-macro))
   (hyper '(manual . op-defmacro))
   (hyper '(manual . class-void))
   (hyper '(manual . const-void))
   (hyper '(manual . class-ign))
   (hyper '(manual . const-ign))))

(define-manual-class (manual . class-fexpr)
  (:title "FEXPR"))

(define-manual-special (manual . op-vau)
  (:title "VAU")
  (:syntax "operand-tree environment-parameter form* => fexpr"))

(define-manual-special (manual . op-deffexpr)
  (:title "DEFFEXPR")
  (:syntax "name operand-tree environment-parameter form* => fexpr"))

(define-manual-class (manual . class-function)
  (:title "FUNCTION"))

(define-manual-function (manual . op-wrap)
  (:title "WRAP")
  (:syntax "fexpr => function"))

(define-manual-function (manual . op-unwrap)
  (:title "UNWRAP")
  (:syntax "function => fexpr"))

(define-manual-special (manual . op-lambda)
  (:title "LAMBDA")
  (:syntax "parameter-tree form* => function"))

(define-manual-special (manual . op-defun)
  (:title "DEFUN")
  (:syntax "name parameter-tree form* => function"))

(define-manual-special (manual . op-function)
  (:title "FUNCTION")
  (:syntax "name => function"))

(define-manual-function (manual . op-eval)
  (:title "EVAL")
  (:syntax "form environment => result"))

(define-manual-function (manual . op-apply)
  (:title "APPLY")
  (:syntax "function arguments => result"))

(define-manual-special (manual . op-funcall)
  (:title "FUNCALL")
  (:syntax "function argument* => result"))

(define-manual-special (manual . op-quote)
  (:title "QUOTE")
  (:syntax "form => form"))

(define-manual-class (manual . class-macro)
  (:title "MACRO"))

(define-manual-special (manual . op-macro)
  (:title "MACRO")
  (:syntax "operand-tree form* => macro"))

(define-manual-special (manual . op-defmacro)
  (:title "DEFMACRO")
  (:syntax "name operand-tree form* => macro"))

(define-manual-class (manual . class-void)
  (:title "VOID"))

(define-manual-constant (manual . const-void)
  (:title "#VOID")
  (:syntax "#void"))

(define-manual-class (manual . class-ign)
  (:title "IGN"))

(define-manual-constant (manual . const-ign)
  (:title "#IGN")
  (:syntax "#ign"))


(define-manual-section (manual . sec-environments)
  (:title "Environments")
  (:child
   (hyper '(manual . class-environment))
   (hyper '(manual . op-def))
   (hyper '(manual . op-defconstant))
   (hyper '(manual . op-setq))
   (hyper '(manual . op-let))
   (hyper '(manual . op-let-star))
   (hyper '(manual . op-flet))
   (hyper '(manual . op-labels))
   (hyper '(manual . op-make-environment))
   (hyper '(manual . op-the-environment))))

(define-manual-class (manual . class-environment)
  (:title "ENVIRONMENT"))

(define-manual-special (manual . op-def)
  (:title "DEF")
  (:syntax "definiend value => result"))

(define-manual-special (manual . op-defconstant)
  (:title "DEFCONSTANT")
  (:syntax "definiend value => result"))

(define-manual-special (manual . op-setq)
  (:title "SETQ")
  (:syntax "definiend value => result"))

(define-manual-special (manual . op-let)
  (:title "LET")
  (:syntax "((var value)*) body* => result"))

(define-manual-special (manual . op-let-star)
  (:title "LET*")
  (:syntax "((var value)*) body* => result"))

(define-manual-special (manual . op-flet)
  (:title "FLET")
  (:syntax "((function-name lambda-list form*)*) body* => result"))

(define-manual-special (manual . op-labels)
  (:title "LABELS")
  (:syntax "((function-name lambda-list form*)*) body* => result"))

(define-manual-function (manual . op-make-environment)
  (:title "MAKE-ENVIRONMENT")
  (:syntax "[parent] => environment"))

(define-manual-special (manual . op-the-environment)
  (:title "THE-ENVIRONMENT")
  (:syntax "=> current-environment"))


(define-manual-section (manual . sec-objects)
  (:title "Objects")
  (:child
   (hyper '(manual . class-object))
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

(define-manual-class (manual . class-object)
  (:title "OBJECT"))

(define-manual-special (manual . op-defstruct)
  (:title "DEFSTRUCT")
  (:syntax "name slot-name* => void"))

(define-manual-function (manual . op-make-instance)
  (:title "MAKE-INSTANCE")
  (:syntax "class . initargs => instance"))

(define-manual-function (manual . op-class-of)
  (:title "CLASS-OF")
  (:syntax "instance => class"))

(define-manual-function (manual . op-find-class)
  (:title "FIND-CLASS")
  (:syntax "symbol => class"))

(define-manual-special (manual . op-class)
  (:title "CLASS")
  (:syntax "name => class"))

(define-manual-function (manual . op-slot-value)
  (:title "SLOT-VALUE")
  (:syntax "instance slot-name => value"))

(define-manual-function (manual . op-set-slot-value)
  (:title "SET-SLOT-VALUE")
  (:syntax "instance slot-name slot-value => value"))

(define-manual-function (manual . op-slot-boundp)
  (:title "SLOT-BOUND?")
  (:syntax "instance slot-name => boolean"))

(define-manual-function (manual . op-eq)
  (:title "EQ")
  (:syntax "value1 value2 => result"))

(define-manual-function (manual . op-eql)
  (:title "EQL")
  (:syntax "value1 value2 => result"))

(define-manual-function (manual . op-typep)
  (:title "TYPE?")
  (:syntax "object type-spec => result"))

(define-manual-special (manual . op-typecase)
  (:title "TYPECASE")
  (:syntax "object (type-spec form*)* => result"))

(define-manual-special (manual . op-the)
  (:title "THE")
  (:syntax "object type => result"))


(define-manual-section (manual . sec-generic-functions)
  (:title "Generic Functions")
  (:child
   (hyper '(manual . op-defgeneric))
   (hyper '(manual . op-defmethod))))

(define-manual-special (manual . op-defgeneric)
  (:title "DEFGENERIC")
  (:syntax "name (param*) => void"))

(define-manual-special (manual . op-defmethod)
  (:title "DEFMETHOD")
  (:syntax "name ((self class) param*) form* => void"))


(define-manual-section (manual . sec-places)
  (:title "Places")
  (:child
   (hyper '(manual . op-setf))
   (hyper '(manual . op-defsetf))
   (hyper '(manual . op-setter))
   (hyper '(manual . op-incf))
   (hyper '(manual . op-decf))))

(define-manual-special (manual . op-setf)
  (:title "SETF")
  (:syntax "place value => result"))

(define-manual-special (manual . op-defsetf)
  (:title "DEFSETF")
  (:syntax "accessor-fn update-fn => void"))

(define-manual-function (manual . op-setter)
  (:title "SETTER")
  (:syntax "function => setter-function"))

(define-manual-special (manual . op-incf)
  (:title "INCF")
  (:syntax "place [increment] => result"))

(define-manual-special (manual . op-decf)
  (:title "DECF")
  (:syntax "place [decrement] => result"))


(define-manual-section (manual . sec-booleans)
  (:title "Booleans")
  (:child
   (hyper '(manual . class-boolean))
   (hyper '(manual . const-t))
   (hyper '(manual . const-f))))

(define-manual-class (manual . class-boolean)
  (:title "BOOLEAN"))

(define-manual-constant (manual . const-t)
  (:title "#T")
  (:syntax "#t"))

(define-manual-constant (manual . const-f)
  (:title "#F")
  (:syntax "#f"))

(define-manual-section (manual . sec-numbers)
  (:title "Numbers")
  (:child
   (hyper '(manual . class-number))
   (hyper '(manual . op-lt))
   (hyper '(manual . op-lte))
   (hyper '(manual . op-gt))
   (hyper '(manual . op-gte))
   (hyper '(manual . op-add))
   (hyper '(manual . op-sub))
   (hyper '(manual . op-mul))
   (hyper '(manual . op-div))))

(define-manual-class (manual . class-number)
  (:title "NUMBER"))

(define-manual-function (manual . op-lt)
  (:title "<")
  (:syntax "number+ => result"))

(define-manual-function (manual . op-lte)
  (:title "<=")
  (:syntax "number+ => result"))

(define-manual-function (manual . op-gt)
  (:title ">")
  (:syntax "number+ => result"))

(define-manual-function (manual . op-gte)
  (:title ">=")
  (:syntax "number+ => result"))

(define-manual-function (manual . op-add)
  (:title "+")
  (:syntax "number* => result"))

(define-manual-function (manual . op-sub)
  (:title "-")
  (:syntax "number+ => result"))

(define-manual-function (manual . op-mul)
  (:title "*")
  (:syntax "number* => result"))

(define-manual-function (manual . op-div)
  (:title "/")
  (:syntax "number+ => result"))


(define-manual-section (manual . sec-strings)
  (:title "Strings")
  (:child
   (hyper '(manual . class-string))))

(define-manual-class (manual . class-string)
  (:title "STRING"))


(define-manual-section (manual . sec-symbols)
  (:title "Symbols")
  (:child
   (hyper '(manual . class-symbol))
   (hyper '(manual . op-make-symbol))
   (hyper '(manual . op-symbol-name))
   (hyper '(manual . op-function-symbol))
   (hyper '(manual . op-type-symbol))))

(define-manual-class (manual . class-symbol)
  (:title "SYMBOL"))

(define-manual-function (manual . op-make-symbol)
  (:title "MAKE-SYMBOL")
  (:syntax "string => symbol"))

(define-manual-function (manual . op-symbol-name)
  (:title "SYMBOL-NAME")
  (:syntax "symbol => string"))

(define-manual-function (manual . op-function-symbol)
  (:title "FUNCTION-SYMBOL")
  (:syntax "symbol => function-symbol"))

(define-manual-function (manual . op-type-symbol)
  (:title "TYPE-SYMBOL")
  (:syntax "symbol => type-symbol"))


(define-manual-section (manual . sec-lists)
  (:title "Lists")
  (:child
   (hyper '(manual . class-nil))
   (hyper '(manual . const-nil))
   (hyper '(manual . class-cons))
   (hyper '(manual . op-cons))
   (hyper '(manual . op-car))
   (hyper '(manual . op-cdr))
   (hyper '(manual . op-cxxr))
   (hyper '(manual . op-list))
   (hyper '(manual . op-list-star))
   (hyper '(manual . op-reverse-list))))

(define-manual-class (manual . class-nil)
  (:title "NIL"))

(define-manual-constant (manual . const-nil)
  (:title "#NIL")
  (:syntax "#nil"))

(define-manual-class (manual . class-cons)
  (:title "CONS"))

(define-manual-function (manual . op-cons)
  (:title "CONS")
  (:syntax "car cdr => cons"))

(define-manual-function (manual . op-car)
  (:title "CAR")
  (:syntax "x => result"))

(define-manual-function (manual . op-cdr)
  (:title "CDR")
  (:syntax "x => result"))

(define-manual-function (manual . op-cxxr)
  (:title "CAAR, CADR, CDAR, CDDR")
  (:syntax "x => result"))

(define-manual-function (manual . op-list)
  (:title "LIST")
  (:syntax "value* => list"))

(define-manual-function (manual . op-list-star)
  (:title "LIST*")
  (:syntax "value* list => list"))

(define-manual-function (manual . op-reverse-list)
  (:title "REVERSE-LIST")
  (:syntax "list => list"))


(define-manual-section (manual . sec-sequences)
  (:title "Sequences")
  (:child
   (hyper '(manual . op-map))
   (hyper '(manual . op-for-each))
   (hyper '(manual . op-subseq))))

(define-manual-function (manual . op-map)
  (:title "MAP")
  (:syntax "function sequence => sequence"))

(define-manual-function (manual . op-for-each)
  (:title "FOR-EACH")
  (:syntax "function sequence => void"))

(define-manual-function (manual . op-subseq)
  (:title "SUBSEQ")
  (:syntax "sequence start [end] => sequence"))


(define-manual-section (manual . sec-control)
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
   (hyper '(manual . op-until))
   (hyper '(manual . op-dotimes))))

(define-manual-special (manual . op-progn)
  (:title "PROGN")
  (:syntax "form* => result"))

(define-manual-special (manual . op-prog1)
  (:title "PROG1")
  (:syntax "form form* => result"))

(define-manual-special (manual . op-prog2)
  (:title "PROG2")
  (:syntax "form form form* => result"))

(define-manual-special (manual . op-if)
  (:title "IF")
  (:syntax "test consequent alternative => result"))

(define-manual-special (manual . op-when)
  (:title "WHEN")
  (:syntax "test form* => result"))

(define-manual-special (manual . op-unless)
  (:title "UNLESS")
  (:syntax "test form* => result"))

(define-manual-special (manual . op-cond)
  (:title "COND")
  (:syntax "(test form*)* => result"))

(define-manual-special (manual . op-case)
  (:title "CASE")
  (:syntax "keyform (key form*)* => result"))

(define-manual-special (manual . op-and)
  (:title "AND")
  (:syntax "form* => result"))

(define-manual-special (manual . op-or)
  (:title "OR")
  (:syntax "form* => result"))

(define-manual-special (manual . op-not)
  (:title "NOT")
  (:syntax "value => result"))

(define-manual-special (manual . op-block)
  (:title "BLOCK")
  (:syntax "tag form* => result"))

(define-manual-special (manual . op-return-from)
  (:title "RETURN-FROM")
  (:syntax "tag [value] => |"))

(define-manual-special (manual . op-unwind-protect)
  (:title "UNWIND-PROTECT")
  (:syntax "protected-form cleanup-form* => result"))

(define-manual-special (manual . op-loop)
  (:title "LOOP")
  (:syntax "form* => |"))

(define-manual-special (manual . op-while)
  (:title "WHILE")
  (:syntax "test form* => |"))

(define-manual-special (manual . op-until)
  (:title "UNTIL")
  (:syntax "test form* => |"))

(define-manual-special (manual . op-dotimes)
  (:title "DOTIMES")
  (:syntax "(var count-form [result-form]) form* => result"))


(define-manual-section (manual . sec-continuations)
  (:title "Delimited Continuations")
  (:child
   (hyper '(manual . class-continuation))
   (hyper '(manual . op-push-prompt))
   (hyper '(manual . op-take-subcont))
   (hyper '(manual . op-push-subcont))
   (hyper '(manual . op-push-prompt-subcont))
   (hyper '(manual . op-push-default-prompt))
   (hyper '(manual . op-take-default-subcont))
   (hyper '(manual . op-push-default-subcont))))

(define-manual-class (manual . class-continuation)
  (:title "CONTINUATION"))

(define-manual-special (manual . op-push-prompt)
  (:title "PUSH-PROMPT")
  (:syntax "prompt form* => result"))

(define-manual-special (manual . op-take-subcont)
  (:title "TAKE-SUBCONT")
  (:syntax "prompt continuation form* => result"))

(define-manual-special (manual . op-push-subcont)
  (:title "PUSH-SUBCONT")
  (:syntax "continuation form* => result"))

(define-manual-special (manual . op-push-prompt-subcont)
  (:title "PUSH-PROMPT-SUBCONT")
  (:syntax "prompt continuation form* => result"))

(define-manual-special (manual . op-push-default-prompt)
  (:title "PUSH-DEFAULT-PROMPT")
  (:syntax "form* => result"))

(define-manual-special (manual . op-take-default-subcont)
  (:title "TAKE-DEFAULT-SUBCONT")
  (:syntax "continuation form* => result"))

(define-manual-special (manual . op-push-default-subcont)
  (:title "PUSH-DEFAULT-SUBCONT")
  (:syntax "continuation form* => result"))


(define-manual-section (manual . sec-dynamic)
  (:title "Dynamic Variables")
  (:child
   (hyper '(manual . op-defdynamic))
   (hyper '(manual . op-dynamic-bind))
   (hyper '(manual . op-dynamic))))

(define-manual-special (manual . op-defdynamic)
  (:title "DEFDYNAMIC")
  (:syntax "name [value] => void"))

(define-manual-special (manual . op-dynamic-bind)
  (:title "DYNAMIC-BIND")
  (:syntax "((dynamic-var value)*) form* => result"))

(define-manual-special (manual . op-dynamic)
  (:title "DYNAMIC")
  (:syntax "dynamic-var => value"))


(define-manual-section (manual . sec-conditions)
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

(define-manual-special (manual . op-handler-bind)
  (:title "HANDLER-BIND")
  (:syntax "((condition-type handler-function)*) form* => value"))

(define-manual-function (manual . op-signal)
  (:title "SIGNAL")
  (:syntax "condition => value"))

(define-manual-function (manual . op-warn)
  (:title "WARN")
  (:syntax "condition => value"))

(define-manual-function (manual . op-error)
  (:title "ERROR")
  (:syntax "condition => |"))

(define-manual-special (manual . op-restart-bind)
  (:title "RESTART-BIND")
  (:syntax "((restart-name function key-val-pair*)*) form* => result"))

(define-manual-function (manual . op-invoke-restart)
  (:title "INVOKE-RESTART")
  (:syntax "restart-designator argument* => result"))

(define-manual-function (manual . op-invoke-restart-interactively)
  (:title "INVOKE-RESTART-INTERACTIVELY")
  (:syntax "restart-designator => result"))

(define-manual-function (manual . op-invoke-debugger)
  (:title "INVOKE-DEBUGGER")
  (:syntax "condition => |"))

(define-manual-function (manual . op-find-restart)
  (:title "FIND-RESTART")
  (:syntax "restart-designator [associated-condition] => restart"))

(define-manual-function (manual . op-compute-restarts)
  (:title "COMPUTE-RESTARTS")
  (:syntax "[associated-condition] => restarts"))


(define-manual-section (manual . sec-js)
  (:title "JavaScript Interface")
  (:child
   (hyper '(manual . class-js-object))
   (hyper '(manual . class-js-array))
   (hyper '(manual . class-js-function))
   (hyper '(manual . op-js-global))
   (hyper '(manual . op-js-new))
   (hyper '(manual . op-js-object))
   (hyper '(manual . op-js-array))
   (hyper '(manual . op-js-function))
   (hyper '(manual . op-js-lambda))
   (hyper '(manual . op-js-get))
   (hyper '(manual . op-js-set))
   (hyper '(manual . op-js-getter))
   (hyper '(manual . op-js-invoker))
   (hyper '(manual . class-js-null))
   (hyper '(manual . const-null))
   (hyper '(manual . class-js-undefined))
   (hyper '(manual . const-undefined))
   (hyper '(manual . op-list-to-js-array))
   (hyper '(manual . op-plist-to-js-object))
   (hyper '(manual . op-log))))

(define-manual-class (manual . class-js-object)
  (:title "JS-OBJECT"))

(define-manual-class (manual . class-js-array)
  (:title "JS-ARRAY"))

(define-manual-class (manual . class-js-function)
  (:title "JS-FUNCTION"))

(define-manual-class (manual . class-js-null)
  (:title "JS-NULL"))

(define-manual-class (manual . class-js-undefined)
  (:title "JS-UNDEFINED"))

(define-manual-constant (manual . const-null)
  (:title "#NULL")
  (:syntax "#null"))

(define-manual-constant (manual . const-undefined)
  (:title "#UNDEFINED")
  (:syntax "#undefined"))

(define-manual-function (manual . op-js-global)
  (:title "JS-GLOBAL")
  (:syntax "variable-name => value"))

(define-manual-function (manual . op-js-getter)
  (:title "JS-GETTER")
  (:syntax "property-name => getter-function"))

(define-manual-function (manual . op-js-invoker)
  (:title "JS-INVOKER")
  (:syntax "method-name => invoker-function"))

(define-manual-function (manual . op-js-new)
  (:title "JS-NEW")
  (:syntax "constructor argument* => object"))

(define-manual-function (manual . op-js-get)
  (:title "JS-GET")
  (:syntax "object key => value"))

(define-manual-function (manual . op-js-set)
  (:title "JS-SET")
  (:syntax "object key value => value"))

(define-manual-function (manual . op-js-object)
  (:title "JS-OBJECT")
  (:syntax ". initargs => object"))

(define-manual-function (manual . op-js-array)
  (:title "JS-ARRAY")
  (:syntax "value* => array"))

(define-manual-function (manual . op-list-to-js-array)
  (:title "LIST-TO-JS-ARRAY")
  (:syntax "list => array"))

(define-manual-function (manual . op-plist-to-js-object)
  (:title "PLIST-TO-JS-OBJECT")
  (:syntax "plist => object"))

(define-manual-function (manual . op-js-function)
  (:title "JS-FUNCTION")
  (:syntax "function => js-function"))

(define-manual-special (manual . op-js-lambda)
  (:title "JS-LAMBDA")
  (:syntax "lambda-list form* => js-function"))

(define-manual-function (manual . op-log)
  (:title "LOG")
  (:syntax "value* => result"))
