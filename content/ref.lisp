(defnode (ref) +qua-hub-page+
  (:title "References")
  (:child
   (hyper '(ref . kernel))
   (hyper '(ref . caml-shift))
   (hyper '(ref . monadic-framework))
   (hyper '(ref . mainstream-yield))))

(defnode (ref . kernel) +qua-hub-ref+
  (:title "The Kernel Programming Language")
  (:url "https://web.cs.wpi.edu/~jshutt/kernel.html")
  (:authors "John N. Shutt"))

(defnode (ref . caml-shift) +qua-hub-ref+
  (:title "Delimited Control in OCaml, Abstractly and Concretely")
  (:url "http://okmij.org/ftp/continuations/caml-shift.pdf")
  (:authors "Oleg Kiselyov"))

(defnode (ref . monadic-framework) +qua-hub-ref+
  (:title "A Monadic Framework for Delimited Continuations")
  (:url "https://www.cs.indiana.edu/~dyb/pubs/monadicDC.pdf")
  (:authors "R. Kent Dybvig, Simon Peyton Jones, Amr Sabry"))

(defnode (ref . mainstream-yield) +qua-hub-ref+
  (:title "Yield: Mainstream Delimited Continuations")
  (:url "https://www.cs.indiana.edu/~sabry/papers/yield.pdf")
  (:authors "Roshan P. James, Amr Sabry"))

