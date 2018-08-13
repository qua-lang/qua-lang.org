(defnode index
  (:title "Qua: Ultralight Lisp for the Web")
  (:child 'intro
          'manual
          'tools
          'sample))

(defnode intro
  (:title "Introduction to the Qua Project")
  (:byline "Start here if you are new to Qua."))

(defnode manual
  (:title "Qua Language Manual")
  (:byline "Reference for the Lisp dialect implemented by Qua.")
  (:child 'simple-control))

(defnode simple-control
  (:title "Simple Control")
  (:child 'op-progn))

(defconstant +special+ "Special Operator")

(defnode op-progn
  (:title "PROGN")
  (:type +special+))

(defnode tools
  (:title "Qua Tools Guide")
  (:byline "How to use Qua in web pages and Node."))

(defnode sample
  (:title "Qua Demos and Sample Code")
  (:byline "Code already written in Qua, such as the generator for
  this website, for you to look at."))
