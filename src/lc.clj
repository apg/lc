(ns lc
  (:use [clojure.set :only (difference)])
  (:require [name.choi.joshua.fnparse :as p])
  (:require [clojure.contrib.error-kit :as kit]))

(def hat (p/constant-semantics (p/lit \^) :hat))
(def dot (p/constant-semantics (p/lit \.) :dot))
(def ws (p/constant-semantics (p/rep+ (p/lit-alt-seq "\t \n")) :ws))
(def open-paren (p/constant-semantics (p/lit \() :open-paren))
(def close-paren (p/constant-semantics (p/lit \)) :close-paren))

(declare exp)

(def ident (p/complex
            [v (p/lit-alt-seq "abcdefghijklmnopqrstuvwxyz")]
            [:var v]))


;; abs := ^ ident . exp
(def abs (p/complex [_ hat
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
     (:lambda arg body) (create-procedure arg body)       
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


(defn free-variables
  ([exp] (free-variables exp #{}))
  ([exp bound]
     (match exp
            (:var s) (if (bound s)
                       #{}
                       #{s})
            (:application op operand) (hash-set (concat
                                                 (free-variables op bound)
                                                 (free-variables operand bound)))
            (:lambda arg body) (free-variables body (conj bound arg)))))


(defn bound-variables
  ([exp] (bound-variables exp #{}))
  ([exp vars]
     (match exp
            (:lambda arg body) (bound-variables body (conj vars (second arg)))
            (:application op operand) (hash-set (concat
                                                 (bound-variables op vars)
                                                 (bound-variables operand vars)))
            (:var v) vars)))


(defn fresh-var [exp]
  (first (difference (apply hash-set (map char (range 97 123))) (bound-variables exp))))


(defn subst [exp old new]
  (match exp
         (:var v) (if (= exp old)
                    new
                    exp)
         (:application op operand) [:application
                                    (subst op old new)
                                    (subst operand old new)]
         (:lambda arg body) (if (= arg old)
                              exp
                              [:lambda arg (subst body old new)])))


(defn alpha [exp old new]
  (if (= (first exp) :lambda)
    (if (= (second exp) old)
      [:lambda new (subst exp old [:var new])]
      [:lambda (second exp) (alpha exp old new)])
    exp))


(defn beta [exp]
  (match exp
         (:application op operand) (if (= :lambda (first op))
                                     (subst
                                      (reduce
                                       (fn [exp v]
                                         (do
                                           (alpha exp [:var v] [:var (fresh-var exp)])))
                                       (second op)
                                       (free-variables operand))
                                      (second op)
                                      operand)
                                    [:application (beta op) (beta operand)])
         (:lambda arg body) [:lambda arg (beta body)]
         (:var v) exp))

(defn to-str [exp]
  (match exp
         (:application op operand) (str "(" (to-str op) " " (to-str operand) ")")
         (:lambda arg exp) (str "^" (to-str arg) "." (to-str exp))
         (:var v) (str v)))

(comment
  (is? (beta (parse "(^x.x y)")) [:var \y]))