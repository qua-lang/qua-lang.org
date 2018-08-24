(defconstant +qua-repl-url+ "https://qua-lang.github.io/qua-vm/repl/repl.html")
(defconstant +qua-balls-demo-url+ "https://qua-lang.github.io/qua-vm/demo/balls.html")
(defconstant +qua-bundle-demo-url+ "https://github.com/qua-lang/qua-bundle-demo")
(defconstant +qua-js-url+ "https://qua-lang.github.io/qua-vm/build/out/qua.js")
(defconstant +qua-npm-url+ "https://www.npmjs.com/package/qua-vm")
(defconstant +qua-node-js-url+ "https://nodejs.org/")

(defnode (usage) +qua-hub-page+
  (:title "Using Qua")
  (:byline "Learn the many ways of using Qua in HTML pages, web apps, Node, and an online REPL.")
  (:child
   (hyper '(usage . web-repl))
   (hyper '(usage . html-page))
   (hyper '(usage . command-line-repl))
   (hyper '(usage . file))
   (hyper '(usage . node))
   (hyper '(usage . bundle))))

(defnode (usage . web-repl) +qua-hub-section+
  (:title "Use the Web REPL")
  (:child
   (paragraph "The simplest way to get started is to
   use " (weblink (:title "the online REPL") (:url +qua-repl-url+))
   ". Just click on the link to experience Lisp in cyberspace.")))

(defnode (usage . html-page) +qua-hub-section+
  (:title "Use Qua in an HTML Page")
  (:child
   (paragraph "The next simple way, suitable for small projects, is to
   create an HTML page and include the
   file " (weblink (:title "qua.js") (:url +qua-js-url+)) " to
   evaluate Lisp code.")
   (html-sample "
<!DOCTYPE html>
<html>
<head>
<script type='text/javascript' src='https://qua-lang.github.io/qua-vm/build/out/qua.js'></script>
</head>
<body>
<script type='text/javascript'>
qua.vm().eval_string('($alert \"Hello world!\")');
</script>
</body>
</html>
")
   (paragraph "You can also put your Qua code into a <script> tag, see
   the source of " (weblink (:title "this demo page") (:url
   +qua-balls-demo-url+)) ".")))

(defnode (usage . command-line-repl) +qua-hub-section+
  (:title "Use the Command-Line REPL")
  (:child
   (paragraph "For the authentic '60s teletype experience, Qua can also
   be run from Unix-like command-lines.  This requires that you have "
   (weblink (:title "Node.js") (:url +qua-node-js-url+)) " installed.")
   (shell-sample "
$ npm install -g qua-vm
$ qua
Welcome to Qua!
Press ENTER to evaluate the current line in Qua.
> 
")
   (paragraph "The following sections depend on having installed Qua
   with Node."))) 

(defnode (usage . file) +qua-hub-section+
  (:title "Run Qua Code in a File")
  (:child
   (shell-sample "$ qua-run path/to/file.lisp")))

(defnode (usage . node) +qua-hub-section+
  (:title "Use Qua from Node Programs")
  (:child
   (paragraph "Qua is available on NPM as package "
              (weblink (:title "qua-vm") (:url +qua-npm-url+)) ".")
   (js-sample "
var vm = require('qua-vm').vm()
vm.eval_string('(print \"Hello world!\")')")))

(defnode (usage . bundle) +qua-hub-section+
  (:title "Create a Bundle for Web Distribution")
  (:child
   (paragraph "A bundle is a JS file that includes both a Qua VM as
   well as your Lisp code (in a preparsed form) for easy
   distribution. There's a " (weblink (:title "demo repository") (:url
   +qua-bundle-demo-url+)) " that shows how it works, the synopsis
   is:")
   (shell-sample "$ qua-bundle input-file.lisp output-file.js")))
