(ns learn-macro.core
  (:require [clojure.repl :refer [source doc]]
            [clojure.tools.macro :refer [macrolet symbol-macrolet]]))

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

(defn make-initforms
  [bindforms]
  (let [func (fn [b]
               (if (coll? b)
                 (list (first b) (fnext b))
                 (list b nil)))]
    (mapcat func bindforms)))

(defn make-stepforms
  [bindforms]
  (let [func (fn [b]
               (cond (and (coll? b) (= (count b) 3)) (list (nth b 2) (first b))
                     (and (coll? b)) (first b)
                     :else nil))]
    (map func bindforms)))

(defmacro cl-do
  [bindforms [test & result] & body]
  (let [label (gensym)]
    `(loop [~@(make-initforms bindforms)]
         (if ~test
           (do ~@result)
           (do
             ~@body
             (recur ~@(make-stepforms bindforms))))
       )))

(macroexpand-1 '(cl-do ((x 2 inc)
        (y 5 inc))
       ((> x 10) :hehe)
       (+ x y)))

(cl-do ((x 2 inc)
        (y 5 inc))
       ((> x 10) :hehe)
       (println (+ x y)))

(defmacro our-and
  [& args]
  (case (count args)
    0 true
    1 (first args)
    `(if ~(first args)
           (our-and ~@(rest args)))))

(defmacro our-andb
  "This makes nothing in clojure environment"
  [& args]
  (if (empty? args)
    true
    (letfn [(expander [remained]
              (if-not (empty? (rest remained))
                `(if ~(first remained)
                   ~(expander (rest remained)))
                (first remained)))]
      (expander args))))


(defn foo [x y z]
  (list x
        (let [x y]
          (list x z))))


(defmacro foo [x y z]
  `(list ~x
         (let [x# ~y]
           (list x# ~z))))

(foo 1 2 3)


;symbol macro requires a package to add on

(macrolet [(foo [form] `(+ ~form ~form))]
   (foo 2))


(symbol-macrolet [hi (do (println "Howdy")
                       1)]
    (+ hi 2))


(defn avg [& args]
  (/ (apply + args) (count args)))

(defmacro avg [& args]
  `(/ (+ ~@args) ~(count args)))

(macroexpand-1 '(avg 1 2 3 4))

(defmacro nil!
  [x]
  `(def ~x nil))

(nil! apple)

(defmacro our-while
  [test & body]
  `(cl-do ()
           ((not ~test))
           ~@body))

(defn test-while
  [x]
  (our-while (< @x 10)
    (do (println @x)
      (swap! x inc))))

(test-while (atom 0))

(defmacro our-defn
  [name params & body]
  `(do
     (def ~name (fn ~name ~params ~@body))
     '~name))

(our-defn hello
   [x]
  (+ x 2))


(defmacro my-for
  [[var start stop] & body]
  `(cl-do ((~var ~start inc) (limit# ~stop))
          ((> ~var limit#))
          ~@body))

(my-for [x 1 5]
   (print x))

(cl-do ((x 1 inc)) ((> x 10)) (println (+ x 2)))

(def w (atom nil))

(defmacro gripe
  [warning]
  `(do (swap! w concat (list ~warning)) nil))

(defmacro gripe2
  [warning]
  (list 'do (list 'swap! 'w 'concat (list 'list warning)) 'nil))

(defn sample-ratio
  [v w]
  (let [vn (count v)
        wn (count w)]
    (if (or (< vn 2) (< wn 2))
      (gripe "sample < 2")
      (/ vn wn))))


(sample-ratio [1] [4])

(macroexpand-1 '(gripe2 "sample < 2"))

(defmacro pathological
  [& body]
  (let [syms (remove ())]))


(symbol? (read-string "a"))


(defn func [x] (+ x 1))

(defn mac-f [x] (func x))

(defmacro mac-m [x] `(func ~x))

(defmacro mac-u [x] (list 'func x))



(letfn [(func [x] (- x 1))]
  (mac-f 10))

(letfn [(func [x] (- x 1))]
  (mac-m 10))

(let [func #(- % 1)]
  (mac-m 10))

(letfn [(func [x] (- x 1))]
  (mac-u 10))



(defmacro echo
  [& args]
  `'~(concat args (list 'amen)))

(macroexpand-1 '(echo what it is))


(defn foo [] (echo x))

(foo)

(defn our-length
  [x]
  (if (nil? x)
    0
    (inc (our-length (next x)))))

(our-length [1 2 3 4 5 7 7])


(defn ntha
  [n lst]
  (if (= n 0)
    (first lst)
    (ntha (- n 1) (rest lst))))


(defmacro nthb
  [n lst]
  `(if (= ~n 0)
     (first ~lst)
     (nthb (- ~n 1) (rest ~lst))));this is the wrong one


(defmacro nthc
  [n lst]
  `(cl-do ((~'xu ~n dec)
           (~'yu ~lst rest))
           ((= ~'xu 0) (first ~'yu))))

(macroexpand-1 '(nthc 2 [1 2 3 4]))

(defmacro nthd
  [n lst]
  `(nth-fn ~n ~lst))

(defn nth-fn
  [n lst]
  (if (= n 0)
    (first lst)
    (nth-fn (- n 1) (rest lst))))


(nthd 3 [1 2 3 4 5])



(defn or-expand
  [args]
  (if (empty? args)
    nil
    `(let [sym# ~(first args)]
       (if sym#
         sym#
         (or-expand ~(rest args))))))

(defmacro ora
  [& args]
  (or-expand args))

(clojure.pprint/pprint (macroexpand-1 '(ora false false false 3)))


(or false false 3)

(defmacro orb
  [& args]
  (if (empty? args)
    nil
    `(let [sym# ~(first args)]
       (if sym#
         sym#
         (orb ~@ (rest args))))))



(orb false false false 'a)


(defmacro our-let
  [binds & body]
  `((fn ~(mapv first (partition 2 binds)) ~@body) ~@(map second (partition 2 binds))))


(our-let [x 1 y 2]
   (/ x y))


(defmacro when-bind
  [[var expr] & body]
  `(our-let [~var ~expr]
       (when ~var ~@body)))

(when-bind [y 3]
    (+ y 4))

(defmacro test-let
  [var]
  `(let [~var 2
         x# 3]
     (+ ~var x#)))

(test-let u)


(defmacro when-bind*
  [binds & body]
  (let [binds (partition 2 binds)]
    (if (empty? binds)
      `(do ~@body)
      `(let ~(vec (first binds))
         (if ~(ffirst binds)
           (when-bind* ~(flatten (rest binds)) ~@body))))))


(defmacro with-gensyms
  [syms & body]
  `(let ~(vec (apply concat (map (fn [s] `(~s (gensym))) syms)))
     ~@body))
(macroexpand-1 '(with-gensyms [a b c] body))


(defmacro try-with-gensyms
  [var body]
  `(with-gensyms [a b]
     (let [a 2 b 3 ~var 4]
       (+ a b ~@body))))

(defmacro nif
  [expr pos zero neg]
  `(let [g# ~expr]
     (cond (pos? g#) ~pos
           (zero? g#) ~zero
           :else ~neg)))

(map #(nif % 'p 'z 'n) [-3 2 7 0])

(defmacro in
  [obj & choices]
  (let [insym# obj]
     `(or ~@(map #(= insym# %) choices))))

(in 3 3 4 5)

(in 3 4 5 6)

(in [3 4] [4 5] [3 4] [5 6])

(defmacro what-is-this?
  [a]
  `'~a)

(what-is-this? [a b])

(what-is-this? 3)

(defmacro inq
  [obj & args]
  `(in ~obj ~@(map (fn [a] `'~a) args)))

(inq 'a a b c)

(defmacro in-if
  [func & choices]
  (let [func# func]
     `(or ~@(map (fn [x]
                   `(~func# ~x))
                 choices))))

(defmacro testit []
  `(let [x# 4] x#))

(testit)

(defmacro testagain []
  (let [x# 4]
    `[~x#]))

(testagain)

(defn >casex
  [g cl]
  (let [k (first cl)
        r (rest cl)]
    (cond (list? k) `((in ~g ~@k) ~@r)
          (inq k true :else) `(true ~@r)
          true (throw (Exception. "Fuck! You've crossed the line!")))))

(defmacro >case
  [expr & clauses]
  (let [g (gensym)]
  `(let [~g ~expr]
     (cond ~@(->> (map (fn [cl] (>casex g cl)) clauses)
                  ((partial apply concat)))))))

(>case (a b)
   ('(a b) 'haha)
   ('(b c) 'hehe)
   (':else 'huahua))

(defmacro is-list?
  [a]
  (list? a))

(is-list? (x 2))




































