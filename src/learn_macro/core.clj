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

(nil!3 'yyy)

