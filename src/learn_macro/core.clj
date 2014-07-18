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

(defmacro our-when
  [test & body]
  `(if ~test
     (do ~@body)))

(defmacro my-when
  [test & body]
  (list 'if test (cons 'do body)))

(my-when (> 3 2) (println 'a) (println 'b))

(list* 'a 'b '(c d))

(defn member
  [a list & {:keys [test]}]
  (if (test a (first list))
    list
    (member a (rest list) :test test)))

(defmacro memq
  [a coll]
  `(member ~a ~coll :test =))

(macroexpand-1 '(my-when (> 3 2) (println 'a) (println 'b)))

(clojure.pprint/pprint (macroexpand-1 '(while (> 3 2) (println 'a) (println 'b))))

(defmacro mac
  [exp]
  `(clojure.pprint/pprint (macroexpand-1 '~exp)))

(mac (while (> 3 2) (println 'a) (println 'b)))

(let [a (macroexpand-1 '(memq 3 [1 2 3 4 5]))]
  (eval a))

(defmacro our-doseq
  [[var sq & return] & body]
  `(do (doall (map (fn [~var] ~@body) ~sq))
     (let [~var nil]
       ~return)))

(our-doseq
 [x [1 2 3]]
 (println x))

(defmacro when-bind
  [[var expr] & body]
  `(let [~var ~expr]
     (when ~var
       ~@body)))

(when-bind [x true] x)

(defmacro cl-do
  [bindforms [test & result] & body]
  (let [label (gensym)]
    `(loop [label ~(make-initforms bindforms)]
         (if ~test
           (do ~@result)
           (do (~@body)
             (recur ~(make-stepforms bindforms)))))))










