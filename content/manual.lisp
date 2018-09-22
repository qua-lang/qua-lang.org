(define-manual (manual)
  (:title "Qua Manual")
  (:byline "Reference for the Lisp dialect implemented by Qua, a blend
  of Kernel, Common Lisp, and Scheme.")
  (:abstract
   (paragraph "Qua is a new dialect of Lisp, designed to be
    exceptionally simple and allow tiny implementations, yet offer the
    powerful metaprogramming facilities, control flow abstractions,
    and general no-nonsense approach that Lisp programmers have come
    to expect.  Qua is based on Kernel, Common Lisp, and Scheme.  From
    Kernel it takes its central computing workhorses, lexically-scoped
    fexprs and first-class environments.  The surface syntax and core
    language look and feel a lot like Common Lisp, from which Qua
    inherits many operators.  The interface for control flow
    manipulation, delimited continuations, is the result of a long
    line of research pioneered in Scheme.")
   (paragraph "Qua is still unfinished.  Especially the areas of
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


(define-section (manual . sec-syntax)
  (:title "Syntax")
  (:content (paragraph "Apart from some extra syntax for JS-related
  functionality, Qua syntax mostly follows common Lisp syntactic
  conventions and shouldn't be too surprising."))
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
   (hyper '(manual . stx-js-method))
   (hyper '(manual . stx-line-comment))))

(define-manual-syntax (manual . stx-string)
  (:title "String Syntax")
  (:syntax "\"characters\" \\\" \\t \\r \\n \\\\")
  (:content
   (paragraph
    "Syntax for " (hyper '(manual . class-string) "strings") "
    follows " (hyper '(ref . json))
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
    "Syntax for " (hyper '(manual . class-number) "numbers") "
    follows " (hyper '(ref . json)) " but only supports a subset of it
    at the moment."))
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
    "Built-in " (hyper '(manual . concept-constant) "constants") ",
    such as " (hyper '(manual . const-t)) " or " (hyper '(manual
    . const-void)) " get a special syntax distinct from " (hyper
    '(manual . class-symbol) "symbols") "."))
  (:example
   "#t => #t
#void => #void")
  (:rationale
   (paragraph
    "Avoiding to pollute the "
    (hyper '(manual . concept-variable-namespace))
    " with identifiers for constants, which should be short, seems to
    be a good idea.")))

(define-manual-syntax (manual . stx-symbol)
  (:title "Variable Symbol Syntax")
  (:syntax "symbol-name")
  (:content
   (paragraph
    (hyper '(manual . concept-variable-namespace) "Variable symbols")
    " allow many characters except space (exact list to be determined)
    and should allow all alphanumeric Unicode characters at some
    point, but are currently restricted to ASCII."))
  (:example
   "foo-bar*")
  (:rationale
   (paragraph "This is just the usual Lisp symbol syntax.")))

(define-manual-syntax (manual . stx-keyword)
  (:title "Keyword Symbol Syntax")
  (:syntax ":keyword-name")
  (:content
   (paragraph
    (hyper '(manual . concept-keyword-namespace) "Keyword symbols")
    " follow " (hyper '(manual . stx-symbol)) " as to the content of the
    symbol name."))
  (:example
   ":my-keyword => :my-keyword")
  (:rationale
   (paragraph "This is just the usual Lisp keyword syntax.")))

(define-manual-syntax (manual . stx-function)
  (:title "Function Symbol Syntax")
  (:syntax "#'function-name")
  (:content
   (paragraph
    (hyper '(manual . concept-function-namespace) "Function symbols")
    " follow " (hyper '(manual . stx-symbol)) " as to the content of the
    symbol name."))
  (:example
   "#'+
#'quuxify

;; Unlike Common Lisp, Qua universally allows function symbols
;; as definiends and parameters:
(def #'my-function (lambda () 1))
(my-function) => 1

(defun compose (#'f #'g)
  (lambda (x) (f (g x))))")
  (:rationale
   (paragraph "While Qua handles " (hyper '(manual
   . concept-namespace) "namespaces") " differently than existing
   Lisps, it makes sense to keep the familiar syntax for referring to "
   (hyper '(manual . class-function) "functions") ", and more
   generally, " (hyper '(manual . concept-operator) "operators") ".")))

(define-manual-naming-convention (manual . stx-dynamic-variable)
  (:title "Dynamic Variable Naming Convention")
  (:syntax "*dynamic-name*")
  (:content
   (paragraph
    "Names of " (hyper '(manual . class-dynamic) "dynamic
    variables") " are wrapped in ``*'' by convention."))
  (:example
   "*standard-output*")
  (:rationale
   (paragraph "Using special prefixes and suffixes prevents having to
   use prefixes like ``current-'' for dynamic variable names to
   distinguish them from local " (hyper '(manual
   . concept-variable) "variables") ".")))

(define-manual-naming-convention (manual . stx-constant-variable)
  (:title "Constant Variable Naming Convention")
  (:syntax "+constant-name+")
  (:content
   (paragraph
    "Names of " (hyper '(manual . concept-constant-variable) "constant
    variables") " are wrapped in ``+'' by convention.  This should be
    used for " (hyper '(manual . concept-variable) "variables") " that
    are never reassigned, and that are " (hyper '(manual
    . concept-binding) "bound") " to objects whose contents are
    immutable."))
  (:example
   "(defconstant +my-constant+ 23)")
  (:rationale
   (paragraph "Common Lisp convention.")))

(define-manual-naming-convention (manual . stx-global-variable)
  (:title "Global Variable Naming Convention")
  (:syntax "-global-name-")
  (:content
   (paragraph
    "Names of " (hyper '(manual . concept-global-variable) "global
    variables") " are wrapped in ``-'' by convention.  This should be
    used for " (hyper '(manual . concept-variable) "variables") " that
    are reassigned, like a global counter, or that are " (hyper
    '(manual . concept-binding) "bound") " to objects whose contents
    are mutated, like a global hash table."))
  (:example "(def -my-global-hash-table- (js-object))")
  (:rationale
   (paragraph "It seems like a good idea to syntactically distinguish
   global variables, analogous to dynamic variables.")))

(define-manual-syntax (manual . stx-list)
  (:title "List Syntax")
  (:syntax "( element* )")
  (:content
   (paragraph
    "The usual syntax for " (hyper '(manual . concept-list) "lists") "."))
  (:example "(a big (nested (list)))

() === #nil")
  (:rationale (paragraph "Proven syntax since 1958.")))

(define-manual-syntax (manual . stx-dotted-list)
  (:title "Dotted List Syntax")
  (:syntax "( element+ . element )")
  (:content
   (paragraph
    "The usual syntax for specifying the last element of a "
    (hyper '(manual . concept-list)) " explicitly."))
  (:example "(a dotted . list)

(1 2) === (1 . (2)) === (1 . (2 . #nil))")
  (:rationale (paragraph "Proven syntax since 1958.")))

(define-manual-syntax (manual . stx-quote)
  (:title "Quotation Syntax")
  (:syntax "'form")
  (:content
   (paragraph
    "The usual syntax for preventing " (hyper '(manual
    . sec-evaluation) "evaluation") " of a " (hyper '(manual
    . concept-form)) ", syntactic sugar for " (hyper '(manual
    . op-quote)) "."))
  (:example "'foo => foo
'#'foo => #'foo
'12 => 12
''foo => 'foo
'(+ 3 4) => (+ 3 4)")
  (:rationale (paragraph "Proven syntax since 1958.")))

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
   ";; Access global JavaScript variables:
$window => #[js-object]

;; Call global JS functions:
($alert \"Hello world!\") => #void

;; Update global JavaScript variables:
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
   (paragraph "See rationale for " (hyper '(manual . stx-js-global)) ".")))

(define-manual-syntax (manual . stx-js-method)
  (:title "JS Method Syntax")
  (:syntax "@method")
  (:content
   (paragraph
    "Syntax for invoking a JavaScript method.  This is syntactic sugar
    for " (hyper '(manual . op-js-invoker)) "."))
  (:example
   "(@toString 12) => \"12\"

(@log $console \"This is a log message sent to the browser console\")

;; Like .property syntax, this returns a function, so we can
;; do things like:
(map @toString (list 1 2 3)) => (\"1\" \"2\" \"3\")")
  (:rationale
   (paragraph "See rationale for " (hyper '(manual . stx-js-global)) ".")))

(define-manual-syntax (manual . stx-line-comment)
  (:title "Line Comment Syntax")
  (:syntax "; comment")
  (:content
   (paragraph
    "Syntax for comments until the end of the line."))
  (:example
   "; this is a comment

;;; this is also a comment")
  (:rationale (paragraph "Classic Lisp.")))


(define-section (manual . sec-evaluation)
  (:title "Evaluation")
  (:content
   (paragraph (term "Evaluation") " is the process of turning
   a " (hyper '(manual . concept-form)) " into a " (hyper '(manual
   . concept-value)) ".  Evaluation happens either implicitly, e.g. at
   the REPL, or explicitly, under programmer control via "
   (hyper '(manual . op-eval)) "."))
  (:child
   (hyper '(manual . concept-form))
   (hyper '(manual . concept-self-evaluating-form))
   (hyper '(manual . concept-constant))
   (hyper '(manual . concept-identifier-form))
   (hyper '(manual . concept-compound-form))
   (hyper '(manual . concept-operator))
   (hyper '(manual . concept-special-operator))
   (hyper '(manual . concept-operand))
   (hyper '(manual . concept-argument))
   (hyper '(manual . concept-parameter))
   (hyper '(manual . concept-environment-parameter))
   (hyper '(manual . concept-value))
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
   (hyper '(manual . concept-macro))
   (hyper '(manual . op-macro))
   (hyper '(manual . op-defmacro))
   (hyper '(manual . class-void))
   (hyper '(manual . const-void))
   (hyper '(manual . class-ign))
   (hyper '(manual . const-ign))))


(define-manual-concept (manual . concept-form)
  (:title "Form")
  (:content
   (paragraph
    "A " (term "form") " is any " (hyper '(manual . class-object)) "
  meant to be " (hyper '(manual . sec-evaluation) "evaluated") ".  It
  is either a " (hyper '(manual . concept-self-evaluating-form)) ",
  an "
  (hyper '(manual . concept-identifier-form)) ", or a "
  (hyper '(manual . concept-compound-form)) "."))
  (:example ";; Self-evaluating forms:
#t => #t
12 => 12
\"foo\" => \"foo\"
:key => :key

(def x 1)
x => 1 ; Identifier form

;; Compound forms:
(+ 1 2) => 3
(if #t 1 2) => 1")
  (:rationale
   (paragraph "Classic Lisp.")))

(define-manual-concept (manual . concept-self-evaluating-form)
  (:title "Self-Evaluating Form")
  (:content
   (paragraph (hyper '(manual . class-object) "Objects") " like "
    (hyper '(manual . concept-constant) "constants") ", " 
    (hyper '(manual . class-string) "strings") ", " 
    (hyper '(manual . class-number) "numbers") ", and " 
    (hyper '(manual . concept-keyword-namespace) "keyword symbols")
    " are said to be " (term "self-evaluating forms") ", that is, they
    simply "
    (hyper '(manual . sec-evaluation) "evaluate") " to themselves."))
    (:example "#t => #t
12 => 12
\"foo\" => \"foo\"
:key => :key")
  (:rationale
   (paragraph "Classic Lisp.")))

(define-manual-concept (manual . concept-constant)
  (:title "Constant")
  (:content
   (paragraph "A " (term "constant") " is a built-in " (hyper '(manual
   . class-object)) " that is a " (hyper '(manual
   . concept-self-evaluating-form)) ".  It is not possible for the Qua
   programmer to define new constants.  Constants are distinct
   from " (hyper '(manual . concept-constant-variable) "constant
   variables") ", which are simply " (hyper '(manual
   . concept-variable) "variables") " that are (hopefully) never
   modified.")
  (paragraph "Core Qua has the following constants: "
             (hyper '(manual . const-nil)) ", "
             (hyper '(manual . const-t)) ", "
             (hyper '(manual . const-f)) ", "
             (hyper '(manual . const-void)) ", and "
             (hyper '(manual . const-ign)) ". In addition, there are "
             (hyper '(manual . const-null)) " and "
             (hyper '(manual . const-undefined)) " from JavaScript."))
  (:example "#t => #t
#void => #void")
  (:rationale
   (paragraph "Classic Lisp.")))

(define-manual-concept (manual . concept-identifier-form)
  (:title "Identifier Form")
  (:content
   (paragraph "A " (hyper '(manual . class-symbol)) " used as a "
              (hyper '(manual . concept-form)) " is called
              an " (term "identifier form") " and "
              (hyper '(manual . sec-evaluation) "evaluates") " to the
              value of the " (hyper '(manual . concept-binding)) "
              identified by the symbol.  An error is signaled if the
              symbol is " (hyper '(manual
              . concept-binding) "unbound") "."))
  (:example "
(def x 1)
x => 1 ; Variable binding

(defun x () 1)
#'x => #[function] ; Function binding
")
  (:rationale
   (paragraph "Classic Lisp, with the difference that Qua treats the "
   (hyper '(manual . concept-variable-namespace)) " and " (hyper
   '(manual . concept-function-namespace)) " uniformly.")))

(define-manual-concept (manual . concept-compound-form)
  (:title "Compound Form")
  (:content
   (paragraph "A " (hyper '(manual . class-cons)) " used as a "
   (hyper '(manual . concept-form)) " is called a " (term "compound
   form") ".")
   (paragraph "If the " (hyper '(manual . op-car)) " of the compound
   form is a " (hyper '(manual . class-symbol)) ", it gets looked up
   in the " (hyper '(manual . concept-function-namespace)) ".")
   (paragraph "Otherwise, the " (hyper '(manual . op-car)) "
   gets " (hyper '(manual . sec-evaluation) "evaluated") " normally.")
   (paragraph "In either case, the resulting value must be an " (hyper
   '(manual . concept-operator)) ", or an error is signalled.")
   (paragraph "Finally, the " (hyper '(manual . op-cdr)) " of the
   compound form is passed as " (hyper '(manual . concept-operand)) "
   to the operator, and the resulting " (hyper '(manual
   . concept-value)) " is returned."))
  (:example ";;; Symbol as first element:

;; Even though + is a variable symbol, it gets turned into a function 
;; symbol and then looked up.
(+ 1 2) => 3

;; It is also possible, although never done in practice,
;; to specify a function symbol as first element explicitly
;; and write it like this:
(#'+ 1 2) => 3

;;; Non-symbol as first element:

;; The LAMBDA gets evaluated normally and then receives (1 2) as operands
((lambda (x y) (+ x y)) 1 2) => 3

;; Unlike Common Lisp, and like Kernel and Scheme, Qua allows any 
;; expression as the first element of a compound form, not only LAMBDA
;; expressions:
(defun my-function-returning-function ()
  (lambda (x y) (+ x y)))
((my-function-returning-function) 1 2) => 3
")
  (:rationale
   (paragraph "Evaluation of compound forms in Qua can be viewed as a
   mix of the evaluation rules of Common Lisp on the one hand, and
   Kernel and Scheme on the other.  Like Common Lisp, Qua treats
   symbols used as the first element of a compound form specially, by
   looking them up in the " (hyper '(manual
   . concept-function-namespace))".  But like Kernel and Scheme, Qua
   places no restrictions on non-symbols used as the first element,
   leading to slightly more expressivity.")))

(define-manual-concept (manual . concept-operator)
  (:title "Operator")
  (:content
   (paragraph "All computation in Qua is performed
   by " (term "operators") ".  All operators have in common that they
   receive " (hyper '(manual . concept-operand) "operands") " and
   return a " (hyper '(manual . concept-value)) ".")
   (paragraph "Operators can be classified as "
              (hyper '(manual . concept-special-operator) "special operators") " ("
              (hyper '(manual . class-fexpr) "fexprs") " and "
              (hyper '(manual . concept-macro) "macros") ") and "
              (hyper '(manual . class-function) "functions") "."))
  (:example ";; + evaluates to a function operator.
;; It receives the list (1 2 3 4) as operands.
;; It returns the value 10.
(+ 1 2 3 4) => 10")
  (:rationale
   (paragraph "Qua operators are what Kernel calls combiners.  I just
   couldn't get myself to use this word, so I chose operator instead,
   since this term is used in Common Lisp for a similar concept.")))

(define-manual-concept (manual . concept-special-operator)
  (:title "Special Operator")
  (:content
   (paragraph "A " (term "special operator") " is an " (hyper '(manual
   . concept-operator)) " that, unlike a " (hyper '(manual
   . class-function)) ", uses special rules to determine which of
   its " (hyper '(manual . concept-operand) "operands") " are "
   (hyper '(manual . sec-evaluation) "evaluated") ".")
   (paragraph "Special operators may be "
              (hyper '(manual . class-fexpr) "fexprs") " or "
              (hyper '(manual . concept-macro) "macros") "."))
  (:example ";; IF is a special operator.  In this case,
;; the form (* 3 3) is never evaluated.
(if #t (* 2 2) (* 3 3)) => 4")
  (:rationale
   (paragraph "Qua uses the term special operator to mean any operator
   with special evaluation rules.  The reason is that users need not
   be aware of how a special operator is implemented, be it as a fexpr
   or macro, only that it has special evaluation rules.")))

(define-manual-concept (manual . concept-operand)
  (:title "Operand")
  (:content
   (paragraph "The " (term "operands") " are the data passed as input to
   an " (hyper '(manual . concept-operator)) ".  If the operator is a "
   (hyper '(manual . concept-special-operator)) ", the operands are
   passed to it unevaluated.  If the operator is a "
   (hyper '(manual . class-function)) ", then each operand is " 
   (hyper '(manual . sec-evaluation) "evaluated") ", and the result is
   called an " (hyper '(manual . concept-argument)) ".")
   (paragraph "While the operands passed to a function must be
   a " (hyper '(manual . concept-list)) ", there is no such
   requirement for special operators: they simply receive the " (hyper
   '(manual . op-cdr)) " of the " (hyper '(manual
   . concept-compound-form)) ".  In the most general case of the
   operands consisting of nested forms, we therefore speak
   of " (term "operand trees") ".  An operand tree is matched against
   the operator's " (hyper '(manual . concept-parameter) "parameter
   tree") "."))
  (:example ";; A fexpr that receives two unevaluated operands and
;; evaluates them explicitly:
(deffexpr a-fexpr (op1 op2) env
  (* (eval op1 env) (eval op2 env)))
(a-fexpr (+ 1 1) (+ 2 2)) => 8

;; A function that does the same thing -- its operands
;; are automatically evaluated to arguments:
(defun a-function (arg1 arg2)
  (* arg1 arg2))
(a-function (+ 1 1) (+ 2 2)) => 8

;; Unlike most Lisps, and like Kernel, Qua doesn't actually require
;; the operands of an operator to be a list.  While usually it is a
;; list, it is still worthwhile to point this out because it is a
;; fundamental feature of Kernel-like languages.  Here we pass a single
;; number as operand to a fexpr:
((vau x #ign x) . 1) => 1

;; A fexpr that destructures its operand tree (1 ((2))) using the
;; parameter tree (x ((y))):
(deffexpr weird (x ((y))) #ign
  (+ x y))
(weird 1 ((2))) => 3")
  (:rationale
   (paragraph "See " (hyper '(ref . kernel)) ".")))

(define-manual-concept (manual . concept-argument)
  (:title "Argument")
  (:content
   (paragraph "An " (term "argument") " is the result of " (hyper
   '(manual . sec-evaluation) "evaluating") " an " (hyper '(manual
   . concept-operand)) ".")
   (paragraph "Since arguments are simply evaluated operands, we
   sometimes refer to them as operands."))
  (:example ";; Operands to a function are automatically evaluated to arguments.
;; Here (+ 1 1) and (+ 2 2) are the operands to the * function.  They
;; are evaluated to the arguments 2 and 4 before being passed to the
;; function.
(* (+ 1 1) (+ 2 2)) => 8")
  (:rationale
   (paragraph "See " (hyper '(ref . kernel)) ".")))

(define-manual-concept (manual . concept-parameter)
  (:title "Parameter")
  (:content
   (paragraph (term "Parameters") " are the means by which " (hyper '(manual
   . concept-operator) "operators") " receive and assign names to " (hyper
   '(manual . concept-operand) "operands")
   " and " (hyper '(manual . concept-argument) "arguments") " passed
   to them.")
   (paragraph "If a parameter is a " (hyper '(manual
   . class-symbol)) ", the operand is " (hyper '(manual
   . concept-binding) "bound") " to the name of the symbol within the
   body of the operator.")
   (paragraph "If a parameter is a " (hyper '(manual . class-cons)) ",
   the " (hyper '(manual . op-car)) " and " (hyper '(manual
   . op-cdr)) " are recursively treated as parameters, giving rise to
   so-called " (term "parameter trees") " (see example below). These
   allow destructuring of " (hyper '(manual
   . concept-operand) "operand trees") ".")
   (paragraph "If a parameter is " (hyper '(manual . const-nil)) ",
              the operand must also be " (hyper '(manual
              . const-nil)) " or an error is signalled.")
   (paragraph "If a parameter is " (hyper '(manual . const-ign)) ",
   the operand is ignored (not " (hyper '(manual
   . concept-binding) "bound") " to a name)."))
  (:example ";; A function that binds its whole argument list to a single name:
(defun return-args-list args
  args)
(return-args-list 1 2 3 4) => (1 2 3 4)
(return-args-list) => ()

;; A function that binds the first two arguments to the names A and B,
;; and the rest of the arguments to the name C:
(defun rest-args (a b . c)
  (list a b c))
(rest-args 1 2 3 4) => (1 2 (3 4))
(rest-args 1 2) => (1 2 ())

;; A function that allows no arguments (in other words, the operand
;; must be #NIL):
(defun no-args () 1)
(no-args) => 1
;; Same thing as:
(defun no-args #nil 1)

;; It is also possible to deeply destructure operands and arguments by
;; means of parameter trees.  Here, A gets bound to the first
;; argument, 1.  B gets bound to the first element of the second
;; argument (which is a list), 2.  C gets bound to the rest of the
;; second argument, (3 4).
(defun destructure (a (b . c))
  (list a b c))
(destructure 1 '(2 3 4)) => (1 2 (3 4))

;; A function that ignores everything past the first argument:
(defun ignore (a . #ign) a)
(ignore 1 2 3 4) => 1")
  (:rationale
   (paragraph "See " (hyper '(ref . kernel)) ".")))

(define-manual-concept (manual . concept-environment-parameter)
  (:title "Environment Parameter")
  (:content
   (paragraph "In addition to ordinary " (hyper '(manual
   . concept-parameter) "parameters") ", "
   (hyper '(manual . class-fexpr) "fexprs") " have a special
   parameter, called " (term "environment parameter") ", through which they
   receive the "
   (hyper '(manual . class-environment)) " in which they are called."))
  (:example ";; A fexpr that returns the value that the variable X has
;; in the environment in which the fexpr is called (here the 
;; environment parameter is called ENV but it could be called
;; anything):
(deffexpr lookup-x-in-environment () env
  (eval 'x env))

(def x 1)
(lookup-x-in-environment) => 1
(let ((x 2))
  (lookup-x-in-environment)) => 2

;; The environment parameter can be ignored if unneeded:
(deffexpr other-fexpr () #ign
  12)
(other-fexpr) => 12")
  (:rationale
   (paragraph "See " (hyper '(ref . kernel)) ".")))

(define-manual-concept (manual . concept-value)
  (:title "Value")
  (:content
   (paragraph (term "Value") " is the term for any " (hyper '(manual
   . class-object)) " that is the result of the " (hyper '(manual
   . sec-evaluation) "evaluation") " of a " (hyper '(manual
   . concept-form)) " or that is the value of a " (hyper '(manual
   . concept-binding)) "."))
  (:example ";; The form (+ 1 1) evaluates to the value 2:
(+ 1 1) => 2

(def x 1)
x => 1 ; the variable X is bound to the value 1")
  (:rationale
   (paragraph "Classic Lisp.")))

(define-manual-class (manual . class-fexpr)
  (:title "FEXPR")
  (:content
   (paragraph
    (term "Fexprs") " are the most fundamental type of
    user-definable " (hyper '(manual . concept-operator)) " in Qua:
    both " (hyper '(manual . class-function) "functions") " and "
    (hyper '(manual . concept-macro) "macros") " are derived from
    them.")
   (paragraph
    "A fexpr receives its " (hyper '(manual
    . concept-operand) "operands") " as " (hyper '(manual
    . sec-evaluation) "unevaluated") " " (hyper '(manual
    . concept-form) "forms") " and has to explicitly use " (hyper
    '(manual . op-eval)) " on them if evaluation is desired.")
   (paragraph "In addition to an ordinary " (hyper '(manual
   . concept-parameter) "parameter tree") ", a fexpr also has an " 
   (hyper '(manual . concept-environment-parameter)) " through which
   it receives the " (hyper '(manual . class-environment)) " in which
   it is called."))
  (:example
   ";; A very simple fexpr that simply returns its single operand,
;; analogous to QUOTE:
(deffexpr my-quote (operand) #ign
  operand)
(my-quote (1 2 3)) => (1 2 3)

;; A fexpr that has no ordinary parameter, but makes use of the
;; environment parameter and returns it, analogous to THE-ENVIRONMENT:
(deffexpr my-current-environment () env
  env)
(let ((x 1))
  (def current-env (my-current-environment))
  (eval 'x current-env)) => 1

;; How WHEN could be defined as a fexpr:
(deffexpr my-when (test . body) env
  (eval (list #'if test (list* #'progn body) #void) env))

(my-when (eql #t #t) 1 2 3) => 3
(my-when (eql #t #f) 1 2 3) => #void")
  (:rationale (paragraph "See " (hyper '(ref . kernel)) ".")))

(define-manual-special (manual . op-vau)
  (:title "VAU")
  (:syntax "parameter-tree environment-parameter form* => fexpr")
  (:operands
   (operand
    (:name "parameter-tree")
    (:description "A " (hyper '(manual . concept-parameter) "parameter tree") "."))
   (operand
    (:name "environment-parameter")
    (:description "An " (hyper '(manual . concept-environment-parameter)) "."))
   (operand
    (:name "form")
    (:description "A " (hyper '(manual . concept-form)) "."))
   (operand
    (:name "fexpr")
    (:description "A " (hyper '(manual . class-fexpr)) ".")))
  (:content
   (paragraph (hyper '(manual . op-vau)) " is the constructor of "
              (hyper '(manual . class-fexpr) "fexprs") ", analogous to how "
              (hyper '(manual . op-lambda)) " is the constructor of "
              (hyper '(manual . class-function) "functions") ".")
   (paragraph "It creates a new fexpr with the given " (hyper '(manual
   . concept-parameter) "parameter tree") ", " (hyper '(manual
   . concept-environment-parameter)) ", and body " (hyper '(manual
   . concept-form) "forms") " and returns it."))
  (:example
   ";; Create a simple fexpr that explicitly evaluates its operands:
(def #'my-fexpr (vau (op1 op2) env 
                  (* (eval op1 env) (eval op2 env))))
(my-fexpr (+ 1 1) (+ 2 2)) => 8")
  (:rationale (paragraph "See " (hyper '(ref . kernel)) ".")))

(define-manual-special (manual . op-deffexpr)
  (:title "DEFFEXPR")
  (:syntax "name parameter-tree environment-parameter form* => name")
  (:operands
   (operand
    (:name "name")
    (:description "A " (hyper '(manual . class-symbol)) "."))
   (operand
    (:name "parameter-tree")
    (:description "A " (hyper '(manual . concept-parameter) "parameter tree") "."))
   (operand
    (:name "environment-parameter")
    (:description "An " (hyper '(manual . concept-environment-parameter)) "."))
   (operand
    (:name "form")
    (:description "A " (hyper '(manual . concept-form)) ".")))
  (:content
   (paragraph (hyper '(manual . op-deffexpr)) " defines a named  "
              (hyper '(manual . class-fexpr)) ", analogous to how "
              (hyper '(manual . op-defun)) " defines a named "
              (hyper '(manual . class-function)) "."))
  (:example 
   ";; Define a simple fexpr that explicitly evaluates its operands:
(deffexpr my-fexpr (op1 op2) env 
  (* (eval op1 env) (eval op2 env)))
(my-fexpr (+ 1 1) (+ 2 2)) => 8")
  (:rationale (paragraph "While Kernel has no analogue to " (hyper
  '(manual . op-deffexpr)) ", Qua has it for symmetry with " (hyper
  '(manual . op-defun)) ".")))

(define-manual-class (manual . class-function)
  (:title "FUNCTION")
  (:content
   (paragraph
    "A " (term "function") " is an " (hyper '(manual
    . concept-operator)) " that always " (hyper '(manual
    . sec-evaluation) "evaluates") " its " (hyper '(manual
    . concept-argument) "arguments") ".  Inside each function is
    actually a " (hyper '(manual . class-fexpr)) " that does the
    computational work of the function.  Argument evaluation is
    induced by " (hyper '(manual . op-wrap)) "."))
  (:example
   ";; A simple function defined using LAMBDA:
((lambda (x y) (+ x y)) 1 2) => 3

;; LAMBDA is merely a shorthand for wrapping a fexpr to induce
;; argument evaluation.  The above is operationally equivalent to:
((wrap (vau (x y) #ign (+ x y))) 1 2) => 1 3")
  (:rationale (paragraph "See " (hyper '(ref . kernel)) ".")))

(define-manual-function (manual . op-wrap)
  (:title "WRAP")
  (:syntax "operator => function")
  (:operands
   (operand
    (:name "operator")
    (:description "An " (hyper '(manual . concept-operator)) "."))
   (operand
    (:name "function")
    (:description "A " (hyper '(manual . class-function)) ".")))
  (:content
   (paragraph (hyper '(manual . op-wrap)) " constructs a " (hyper
   '(manual . class-function)) " given an underlying "
   (hyper '(manual . concept-operator)) ", usually a " (hyper '(manual
   . class-fexpr)) ". All Qua functions are created by " (hyper
   '(manual . op-wrap)) ".  So functions can be viewed
   as " (term "wrappers") " that induce " (hyper '(manual
   . concept-argument) "argument") " " (hyper '(manual
   . sec-evaluation) "evaluation") " for their underlying, wrapped
   fexpr."))
  (:example
   ";; A simple QUOTE like-fexpr:
(def #'my-quote (vau (operand) #ign operand))
;; It returns its operands unevaluated:
(my-quote (+ 1 2)) => (+ 1 2))

;; We can WRAP the fexpr inside a function:
(def #'my-wrapped-quote (wrap #'my-quote))
;; The resulting function evaluates the operand, and passes it to MY-QUOTE:
(my-wrapped-quote (+ 1 2)) => 3

;; While usually not done, WRAP can be applied multiple times:
(def foo 12)
(my-quote 'foo) => 'foo
((wrap #'my-quote) 'foo) => foo
((wrap (wrap #'my-quote)) 'foo) => 12")
  (:rationale (paragraph "See " (hyper '(ref . kernel)) ".")))

(define-manual-function (manual . op-unwrap)
  (:title "UNWRAP")
  (:syntax "function => operator")
  (:operands
   (operand
    (:name "function")
    (:description "A " (hyper '(manual . class-function)) "."))
   (operand
    (:name "operator")
    (:description "An " (hyper '(manual . concept-operator)) ".")))
  (:content
   (paragraph
    (hyper '(manual . op-unwrap)) " extracts the " (hyper '(manual
    . concept-operator)) " underlying a " (hyper '(manual
    . class-function)) "."))
  (:example ";; Every function has an underlying operator. We can extract the
;; operator underlying the * function and call it directly:
(def #'times-operator (unwrap #'*))
(times-operator 2 4) => 8

;; Note that the following would not work, because the underlying
;; operator is a fexpr and does not evaluate its arguments (that's
;; what the wrapper does, and we have removed it):
(times-operator (+ 1 1) (+ 2 2))")
  (:rationale (paragraph "See " (hyper '(ref . kernel)) ".")))

(define-manual-special (manual . op-lambda)
  (:title "LAMBDA")
  (:syntax "parameter-tree form* => function")
  (:operands
   (operand
    (:name "parameter-tree")
    (:description "A " (hyper '(manual . concept-parameter) "parameter tree") "."))
   (operand
    (:name "form")
    (:description "A " (hyper '(manual . concept-form)) "."))
   (operand
    (:name "function")
    (:description "A " (hyper '(manual . class-function)) ".")))
  (:content
   (paragraph
    (hyper '(manual . op-lambda)) " creates a new anonymous "
    (hyper '(manual . class-function)) "."))
  (:example
   "((lambda (x y) (+ x y)) 1 2) => 3

;; LAMBDA allows destructuring of its arguments with parameter trees:
((lambda ((val1 . #ign) (val2 . #ign)) (+ val1 val2))
 (list 1 2 3) (list 10 20 30)) => 11

((lambda args args) 1 2 3) => (1 2 3)

((lambda (first second . rest) (list first second rest)) 1 2 3 4) => (1 2 (3 4))
")
  (:rationale (paragraph "Classic Lisp, with the addition of destructuring.")))

(define-manual-special (manual . op-defun)
  (:title "DEFUN")
  (:syntax "name parameter-tree form* => name")
  (:operands
   (operand
    (:name "name")
    (:description "A " (hyper '(manual . class-symbol)) "."))
   (operand
    (:name "parameter-tree")
    (:description "A " (hyper '(manual . concept-parameter) "parameter tree") "."))
   (operand
    (:name "form")
    (:description "A " (hyper '(manual . concept-form)) ".")))
  (:content
   (paragraph (hyper '(manual . op-defun)) " defines a named  "
              (hyper '(manual . class-function)) "."))
  (:example
   "(defun foo (arg1 arg2)
  (+ arg1 arg2))

(foo (+ 1 1) (+ 2 2)) => 6

;; Like LAMBDA, DEFUN allows destructuring:
(defun bar (((a b))) (+ a b))
(bar '((1 2))) => 3")
  (:rationale (paragraph "Classic Lisp, with the addition of destructuring.")))

(define-manual-special (manual . op-function)
  (:title "FUNCTION")
  (:syntax "name => function")
  (:operands
   (operand
    (:name "name")
    (:description "A " (hyper '(manual . class-symbol)) "."))
   (operand
    (:name "function")
    (:description "A " (hyper '(manual . class-function)) ".")))
  (:content
   (paragraph
    (hyper '(manual . op-function)) " looks up a "
    (hyper '(manual . class-function)) " in the " (hyper '(manual
    . concept-function-namespace)) "."))
  (:example "(defun foo () 12)
(function foo) => #[function]

;; This is the same as:
#'foo => #[function]")
  (:rationale (paragraph "Classic Lisp.")))

(define-manual-function (manual . op-eval)
  (:title "EVAL")
  (:syntax "form environment => result")
  (:operands
   (operand
    (:name "form")
    (:description "A " (hyper '(manual . concept-form)) "."))
   (operand
    (:name "environment")
    (:description "An " (hyper '(manual . class-environment)) "."))
   (operand
    (:name "result")
    (:description "A " (hyper '(manual . concept-value)) ".")))
  (:content
   (paragraph
    (hyper '(manual . op-eval)) " " (hyper '(manual
    . sec-evaluation) "evaluates") " a " (hyper '(manual
    . concept-form)) " in an " (hyper '(manual . class-environment)) "."))
  (:example
"(def a 1)
(def b 2)
(eval '(+ a b) (the-environment)) => 3")
  (:rationale (paragraph "Classic Lisp.")))
  
(define-manual-function (manual . op-apply)
  (:title "APPLY")
  (:syntax "function arguments => result")
  (:operands
   (operand
    (:name "function")
    (:description "A " (hyper '(manual . class-function)) "."))
   (operand
    (:name "arguments")
    (:description "A " (hyper '(manual . concept-list)) "."))
   (operand
    (:name "result")
    (:description "A " (hyper '(manual . concept-value)) ".")))
  (:content
   (paragraph
    (hyper '(manual . op-apply)) " applies a " (hyper '(manual
    . class-function)) " to a " (hyper '(manual . concept-list)) " of " 
    (hyper '(manual . concept-argument) "arguments") "."))
  (:example
"(apply #'+ (list 1 2 3)) => 6")
  (:rationale (paragraph "Classic Lisp.")))

(define-manual-function (manual . op-funcall)
  (:title "FUNCALL")
  (:syntax "function argument* => result")
  (:operands
   (operand
    (:name "function")
    (:description "A " (hyper '(manual . class-function)) "."))
   (operand
    (:name "argument")
    (:description "An " (hyper '(manual . concept-argument)) "."))
   (operand
    (:name "result")
    (:description "A " (hyper '(manual . concept-value)) ".")))
  (:content
   (paragraph
    (hyper '(manual . op-funcall)) " calls a " (hyper '(manual
    . class-function)) " with the supplied " (hyper '(manual
    . concept-argument) "arguments") "."))
  (:example
"(funcall #'+ 1 2 3) => 6

;; FUNCALL is less needed in Qua than in Common Lisp because Qua
;; allows arbitrary expressions in the operator position of a 
;; compound form:
(defun function-returning-function () (lambda (x y) (+ x y)))
;; The following two expressions are equivalent in Qua, but the former
;; is preferred for readability:
(funcall (function-returning-function) 1 2) => 3
((function-returning-function) 1 2) => 3")
  (:rationale (paragraph "Classic Lisp.")))

(define-manual-special (manual . op-quote)
  (:title "QUOTE")
  (:syntax "form => form")
  (:operands
   (operand
    (:name "form")
    (:description "A " (hyper '(manual . concept-form)) ".")))
  (:content
   (paragraph
    (hyper '(manual . op-quote)) " prevents " (hyper '(manual
    . sec-evaluation) "evaluation") " of its " (hyper '(manual
    . concept-operand)) "."))
  (:example
"(quote (+ 1 2)) => (+ 1 2)

;; This is the same as:
'(+ 1 2) => (+ 1 2)")
  (:rationale (paragraph "Classic Lisp.")))

(define-manual-concept (manual . concept-macro)
  (:title "Macro")
  (:content
   (paragraph "A " (term "macro") " is a " (hyper '(manual
   . concept-special-operator)) " that works by producing a "
   (hyper '(manual . concept-form)) ", called
   the " (term "expansion") ", which gets " (hyper '(manual
   . sec-evaluation) "evaluated") " in place of the original macro
   form.")
   (paragraph
    "Macros can be viewed as a restricted subset of "
    (hyper '(manual . class-fexpr) "fexprs")
    " that, instead of directly performing a computation, produce a
    form that performs the computation."))
  (:example ";; How WHEN could be implemented in terms of IF:
(defmacro my-when (test . body)
  (list #'if test (list* #'progn body) #void))

(my-when (eql #t #t) 1 2 3) => 3

;; The above call to MY-WHEN is equivalent to the following expansion:
(if (eql #t #t) (progn 1 2 3) #void) => 3")
  (:rationale
   (paragraph "For many simpler metaprogramming tasks, macros are more
   comfortable to write than fexprs, and an implementation may cache
   the macro expansion after the first use.")))

(define-manual-special (manual . op-macro)
  (:title "MACRO")
  (:syntax "parameter-tree form* => macro")
  (:operands
   (operand
    (:name "parameter-tree")
    (:description "A " (hyper '(manual . concept-parameter) "parameter tree") "."))
   (operand
    (:name "form")
    (:description "A " (hyper '(manual . concept-form)) "."))
   (operand
    (:name "macro")
    (:description "A " (hyper '(manual . concept-macro)) ".")))
  (:content
   (paragraph
    (hyper '(manual . op-macro)) " creates a new anonymous "
    (hyper '(manual . concept-macro)) "."))
  (:example
";; A contrived example: given a number X, the macro expansion
;; is (+ X 5):
(map (macro (x) (list #'+ x 5)) '(10 20 30)) => (15 25 35)")
  (:rationale (paragraph "Anonymous macros are rarely used in
  practice, so " (hyper '(manual . op-macro)) " is provided mainly for
  symmetry with " (hyper '(manual . op-vau)) " and " (hyper '(manual
  . op-lambda)) " and also to underscore the first-class nature of
  macros in Qua.")))

(define-manual-special (manual . op-defmacro)
  (:title "DEFMACRO")
  (:syntax "name parameter-tree form* => name")
  (:operands
   (operand
    (:name "name")
    (:description "A " (hyper '(manual . class-symbol)) "."))
   (operand
    (:name "parameter-tree")
    (:description "A " (hyper '(manual . concept-parameter) "parameter tree") "."))
   (operand
    (:name "form")
    (:description "A " (hyper '(manual . concept-form)) ".")))
  (:content
   (paragraph
    (hyper '(manual . op-defmacro)) " defines a named "
    (hyper '(manual . concept-macro)) "."))
  (:example
";; Here's the definition of LAMBDA as a macro from the Qua code:
(defmacro lambda (params . body)
  (list #'wrap (list* #'vau params #ign body)))

(lambda (x y) (+ x y))
;; The above call to LAMBDA therefore has the following expansion:
(wrap (vau (x y) #ign (+ x y)))")
  (:rationale (paragraph "Mix of Classic Lisp and Kernel.")))

(define-manual-class (manual . class-void)
  (:title "VOID")
  (:content
   (paragraph
    (hyper '(manual . class-void)) " is the "
    (hyper '(manual . class-class)) " of the "
    (hyper '(manual . concept-constant)) " "
    (hyper '(manual . const-void)) "."))
  (:example "#void => #void")
  (:rationale (paragraph "See "(hyper '(manual . const-void)) ".")))

(define-manual-constant (manual . const-void)
  (:title "#VOID")
  (:syntax "#void")
  (:content
   (paragraph
    (hyper '(manual . const-void)) " is a "
    (hyper '(manual . concept-constant)) " that is used when a "
    (hyper '(manual . concept-form)) " has no interesting "
    (hyper '(manual . concept-value)) ".  It is the only "
    (hyper '(manual . class-object)) " of "
    (hyper '(manual . class-class)) " "
    (hyper '(manual . class-void)) "."))
  (:example
   ";; An empty PROGN returns #VOID and so does WHEN when the test is false:
(progn) => #void

(when #f 1 2 3) => #void")
  (:rationale (paragraph "I think reusing " (hyper '(manual . const-nil)) " or "
(hyper '(manual . const-f)) " for this purpose would be silly,
although you can try to change my mind.")))

(define-manual-class (manual . class-ign)
  (:title "IGN")
  (:content
   (paragraph
    (hyper '(manual . class-ign)) " is the "
    (hyper '(manual . class-class)) " of the "
    (hyper '(manual . concept-constant)) " "
    (hyper '(manual . const-ign)) "."))
  (:example "#ign => #ign")
  (:rationale (paragraph "See "(hyper '(manual . const-ign)) ".")))

(define-manual-constant (manual . const-ign)
  (:title "#IGN")
  (:syntax "#ign")
  (:content
   (paragraph
    (hyper '(manual . const-ign)) " is a "
    (hyper '(manual . concept-constant)) " that is used in "
    (hyper '(manual . concept-parameter) "parameter trees") " to ignore an "
    (hyper '(manual . concept-operand)) ".  It is the only "
    (hyper '(manual . class-object)) " of "
    (hyper '(manual . class-class)) " "
    (hyper '(manual . class-ign)) "."))
  (:example ";; A function that doesn't care what its arguments are:
(defun idc #ign 33)
(idc) => 33
(idc 'foo 'bar) => 33

;; Ignoring some arguments:
(defun ignore-second-argument (first #ign third) (+ first third))
(ignore-second-argument 1 2 3) => 4")
  (:rationale (paragraph "See " (hyper '(ref . kernel)) ".")))

(define-section (manual . sec-environments)
  (:title "Environments")
  (:child
   (hyper '(manual . concept-definiend))
   (hyper '(manual . concept-binding))
   (hyper '(manual . concept-variable))
   (hyper '(manual . concept-global-variable))
   (hyper '(manual . concept-constant-variable))
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

(define-manual-concept (manual . concept-definiend)
  (:title "Definiend")
  (:content
   (paragraph ""))
  (:example "")
  (:rationale
   (paragraph "")))

(define-manual-concept (manual . concept-variable)
  (:title "Variable")
  (:content
   (paragraph ""))
  (:example "")
  (:rationale
   (paragraph "")))

(define-manual-concept (manual . concept-binding)
  (:title "Binding")
  (:content
   (paragraph ""))
  (:example "")
  (:rationale
   (paragraph "")))

(define-manual-concept (manual . concept-global-variable)
  (:title "Global Variable")
  (:content
   (paragraph ""))
  (:example "")
  (:rationale
   (paragraph "")))

(define-manual-concept (manual . concept-constant-variable)
  (:title "Constant Variable")
  (:content
   (paragraph ""))
  (:example "")
  (:rationale
   (paragraph "")))

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


(define-section (manual . sec-objects)
  (:title "Objects")
  (:child
   (hyper '(manual . class-class))
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

(define-manual-class (manual . class-class)
  (:title "CLASS"))

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


(define-section (manual . sec-generic-functions)
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


(define-section (manual . sec-places)
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


(define-section (manual . sec-booleans)
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

(define-section (manual . sec-numbers)
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


(define-section (manual . sec-strings)
  (:title "Strings")
  (:child
   (hyper '(manual . class-string))))

(define-manual-class (manual . class-string)
  (:title "STRING"))


(define-section (manual . sec-symbols)
  (:title "Symbols")
  (:child
   (hyper '(manual . concept-namespace))
   (hyper '(manual . concept-variable-namespace))
   (hyper '(manual . concept-function-namespace))
   (hyper '(manual . concept-keyword-namespace))
   (hyper '(manual . concept-type-namespace))
   (hyper '(manual . class-symbol))
   (hyper '(manual . op-make-symbol))
   (hyper '(manual . op-symbol-name))
   (hyper '(manual . op-function-symbol))
   (hyper '(manual . op-type-symbol))))

(define-manual-class (manual . class-symbol)
  (:title "SYMBOL"))

(define-manual-concept (manual . concept-namespace)
  (:title "Namespace")
  (:content
   (paragraph "In addition to its " (hyper '(manual
   . op-symbol-name) "name") ", every " (hyper '(manual
   . class-symbol)) " in Qua has a namespace, either the "
   (hyper '(manual . concept-variable-namespace) "variable
   namespace") ", " (hyper '(manual . concept-function-namespace) "function namespace")
   ", " (hyper '(manual . concept-keyword-namespace) "keyword namespace")
   ", or " (hyper '(manual . concept-type-namespace) "type namespace") "."))
  (:example
   ";; Thanks to namespaces, we can have a variable and function with the same name
(def foo 12)
(defun foo () (+ foo 100))
foo => 12
(foo) => 112"))

(define-manual-concept (manual . concept-variable-namespace)
  (:title "Variable Namespace"))

(define-manual-concept (manual . concept-function-namespace)
  (:title "Function Namespace"))

(define-manual-concept (manual . concept-keyword-namespace)
  (:title "Keyword Namespace"))

(define-manual-concept (manual . concept-type-namespace)
  (:title "Type Namespace"))

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


(define-section (manual . sec-lists)
  (:title "Lists")
  (:child
   (hyper '(manual . concept-list))
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

(define-manual-concept (manual . concept-list)
  (:title "List")
  (:content
   (paragraph ""))
  (:example "")
  (:rationale
   (paragraph "")))

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


(define-section (manual . sec-sequences)
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


(define-section (manual . sec-control)
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


(define-section (manual . sec-continuations)
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


(define-section (manual . sec-dynamic)
  (:title "Dynamic Variables")
  (:child
   (hyper '(manual . class-dynamic))
   (hyper '(manual . op-defdynamic))
   (hyper '(manual . op-dynamic-bind))
   (hyper '(manual . op-dynamic))))

(define-manual-class (manual . class-dynamic)
  (:title "DYNAMIC"))

(define-manual-special (manual . op-defdynamic)
  (:title "DEFDYNAMIC")
  (:syntax "name [value] => void"))

(define-manual-special (manual . op-dynamic-bind)
  (:title "DYNAMIC-BIND")
  (:syntax "((dynamic-var value)*) form* => result"))

(define-manual-special (manual . op-dynamic)
  (:title "DYNAMIC")
  (:syntax "dynamic-var => value"))


(define-section (manual . sec-conditions)
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


(define-section (manual . sec-js)
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
