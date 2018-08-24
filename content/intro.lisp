(defnode (intro) +qua-hub-page+
  (:title "Project Introduction")
  (:byline "Qua is a new Lisp designed from scratch to be easily
  embeddable into JavaScript applications and address the problem of
  programming against asynchronous APIs with secret alien
  technology.")
  (:child
   (hyper '(intro . why))
   (hyper '(intro . other))
   (hyper '(intro . co))))

(defnode (intro . why) +qua-hub-section+
  (:title "Why I Had to Create a New Language: Web Programming Needs
  Powerful Control (Like Delimited Continuations)")
  (:child
   (paragraph "The main problem I am addressing with Qua is the
    control stack loss caused by the asynchronous APIs pervasive in
    JavaScript.  At some point I decided I simply wasn't going to
    write any more algorithms in the callback-style model required by
    most JavaScript APIs.  Life's too short for programming without
    control, and the alternatives offered, like promises or the
    ``generator hack'' are almost as bad.  I decided I wanted a very
    lightweight language, directly and seamlessly embedded into
    JavaScript, with some form of advanced control stack manipulation
    facility, like coroutines or continuations, so that I could invert
    the broken async model and let my algorithms do all the things
    that they should be able to do: block on IO, sleep, etc.")
   (paragraph "I studied various concurrency and control flow
    manipulation models and eventually settled on delimited
    continuations as the main control model of Qua.  Delimited
    continuations have emerged as a consensus way to add continuations
    to a language " (hyper '(ref . monadic-framework)) (hyper '(ref
    . caml-shift)) ", and are essentially a more elegant formulation
    of the same functionality as coroutines " (hyper '(ref
    . mainstream-yield)) ". If you understand e.g. Lua coroutines, you
    already understand delimited continuations, too, and just have to
    learn the new interface, which is beautiful and quite
    mind-blowing.  With delimited continuations, Qua programmers can
    at any point get a copy of the control stack as a datum that can
    be stored in a data structure etc.  This means, the current Qua
    computation can be ``parked'' when an asynchronous JS API call is
    about to be done, and later be restarted when Qua gets called
    back.  So all asynchronous APIs can be inverted to a synchronous
    style from the point of view of the Qua programmer.")))

(defnode (intro . other) +qua-hub-section+
  (:title "The Other Language Ingredients: Fexprs, First-Class
  Environments, and a Healthy Dose of Old School Lisp")
  (:child
   (paragraph "Around the same time, I learned about John Shutt's new
   variant of Lisp, called Kernel " (hyper '(ref . kernel)) ", which
   reintroduced an often misapprehended and disparaged language
   construct called the ``fexpr''.  After some investigation and gasps
   I realized that fexprs are the perfect fundament on which to build
   an ultralight language.  A fexpr is like a function but instead of
   receiving the evaluated values of its arguments (as in
   ``call-by-value''), it receives their unevaluated source
   expressions (``call-by-text'') and the first-class lexical
   environment in which the fexpr is called.  It turns out that this
   simple setup offers about the same metaprogramming power as
   Scheme's most advanced hygienic macro systems, but with none of the
   headaches or flashbacks: hygiene is simply achieved with the
   lexical scope already built into the language.  This means that
   once you put the very simple, in fact trivial, concepts of fexprs
   and first-class environments into a language, you have sufficient
   tools to bootstrap the rest of the language from there, because
   fexprs effectively combine the power of functions and hygienic
   macros (and more) into a single construct.  And to top it off,
   fexprs are a lot of fun, too, so it became a no-brainer to use them
   as the basis of Qua.")
   (paragraph "As to the actual look and feel of Qua Lisp, its surface
   syntax and higher-level language, I am following ``classic'' Lisps,
   like Common Lisp and Emacs Lisp.  Qua is not and will never be
   compatible with any of these languages, but many Qua programs look
   almost exactly like them, and vice versa.  Features that Qua Lisp
   shares with these languages are separate function and variable
   namespaces, an object system with generic functions,
   dynamically-scoped variables, generalized references, a condition
   system with restarts, a generic sequence protocol, and a rich set
   of vintage Lisp data and control flow operators.")))

(defnode (intro . co) +qua-hub-section+
  (:title "A Companion Language to JavaScript and Just Good Fun")
  (:child
   (paragraph "Qua is designed to be symbiotic with JavaScript, not
   replace it, and it is my expectation that most Qua programs will
   contain a core of JavaScript code for the performance-critical
   parts and inner loops.  So I put effort into making Qua deeply
   integrate and interoperate with JS and I think it turned out quite
   well.  Qua can directly -- without any FFI declarations -- access
   JS globals, object properties, call methods and functions, and all
   Qua functions can be made callable as normal JS functions.  Qua
   Lisp and JS parts of a program can live in harmony, and switching
   between the two languages is effortless.  Node and NPM give Qua
   programs access to the immense JS ecosystem.  Qua also works with
   the Browserify tool that makes it possible to use many NPM packages
   in the browser.")
   (paragraph "The role I see for Qua is as the high-level
   ``intelligence'' layer of my JavaScript apps, where the complex
   data-dependent logic resides, while the lower-level JavaScript code
   handles the rote data-processing inner loops. Besides tackling the
   problem of asynchronous IO, Qua is also a boon for its
   metaprogramming capabilities, that bring the declarative,
   ``language-oriented'' Lisp programming style to JavaScript.  And of
   course, just being able to use Lisp on the web is a joy in itself,
   and it is my hope that you get to experience that when you use
   Qua. I do.")))
