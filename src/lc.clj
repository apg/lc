(ns lc
  (:require [name.choi.joshua.fnparse :as p])
  (:require [clojure.contrib.error-kit :as kit]))

(def slash (p/constant-semantics (p/lit \\) :slash))
(def dot (p/constant-semantics (p/lit \.) :dot))
(def ws (p/constant-semantics (p/rep+ (p/lit-alt-seq "\t \n")) :ws))
(def open-paren (p/constant-semantics (p/lit \() :open-paren))
(def close-paren (p/constant-semantics (p/lit \)) :close-paren))

(declare exp)

(def ident (p/complex
            [v (p/lit-alt-seq "abcdefghijklmnopqrstuvwxyz")]
            [:var v]))


;; abs := \ ident . exp
(def abs (p/complex [_ slash
                     arg ident
                     _ dot
                     body exp]
                    [:lambda arg body]))

;; pexp := ( exp ) | ( exp exp )
(def pexp (p/complex [_ open-paren
                      op exp
                      _ (p/opt ws)
                      and? (p/opt exp)
                      _ close-paren]
                     (if and?
                       [:application op and?]
                       op)))

;; exp := pexp | abs | ident
(def exp (p/alt pexp abs ident))

(defn parse [s]
  (p/rule-match exp
                #(prn "error:" %) 
                #(prn "leftover:" %)
                {:remainder s}))


(defmacro match
  "shitty pattern matching macro

Example:

(def eval [exp]
  (match exp
     (:var v) v
     (:abstraction arg body) (create-procedure arg body)       
     (:application operator operand) (operator operand)))
"
  [exp & body]
  (let [pairs (partition 2 body)
        tests (map (fn [[t b]] (first t)) pairs)
        bodies (map (fn [[t b]]
                       `(let [[~(gensym) ~@(rest t)] ~exp]
                          ~b))
                    pairs)]
    `(case (first ~exp)
           ~@(mapcat #(vector %1 %2) tests bodies))))


(defn subst [exp old new]
  (match exp
         (:var v) (if (= v old)
                    [:var new]
                    exp)
         (:application op operand) [:application
                                    (subst op old new)
                                    (subst operand old new)]
         (:lambda arg body) (if (= arg old)
                              exp
                              [:lambda arg (subst body old new)])))

(defn beta [exp]
  exp)

(defn alpha [exp]
  exp)

(defn free-variables [exp]
  #{})

(defn bound-variables [exp]
  #{})


(comment
  (is? (beta (parse "(\\x.x y)")) [:var \y]))