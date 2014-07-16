(ns learn-macro.core)

(defmacro nil!
  [x]
  `(def ~x nil))

(defmacro nil!2
  [x]
  (list 'def x nil))

(defmacro nil!3
  [x]
  (list 'def x 'nil))

(eval (list 'def 'xxx 3))

(nil!3 yyy)

(let [b 3]
  `(a ~b c))

(let [b 3]
  `(a (~b c)))

(let [a 1
      b 2
      c 3]
  `(a b ~c ('~(+ a b c)) (+ a b) 'c '((~a ~b))))





(defmacro nif
  [expr pos zero neg]
  `(case (int (Math/signum (double ~expr)))
     1 ~pos
     0 ~zero
     -1 ~neg))

(nif -3 'p 'z 'n)

(nif (- 2 3) 'p 'z 'n)

(let [b '(1 2 3)]
  `(a ~@b c))






