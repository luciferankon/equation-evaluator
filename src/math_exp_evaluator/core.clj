(ns math-exp-evaluator.core
  (:require [instaparse.core :as insta]))

;(def math-expression
;  (insta/parser
;    "<S> = LHS EQUAL RHS
;    <LHS> = E <MAYBESPACE>
;    <RHS> = <MAYBESPACE> E
;    MAYBESPACE = ' ' | EPSILON
;    EQUAL = '='
;    <E> = NUM (E | EPSILON) | VAR (E | EPSILON) | E <MAYBESPACE> OPERATOR <MAYBESPACE> E | PE
;    <OPERATOR> = ADD | SUB | MUL | DIV
;    ADD = <'+'>
;    SUB = <'-'>
;    MUL = <'*'>
;    DIV = <'/'>
;    PE = \"(\" E \")\"
;    NUM = #'[0-9]'+
;    VAR = #'[A-z]'"))

;(def arithmetic
;  (insta/parser
;    "expr = add-sub
;     <add-sub> = mul-div | add | sub
;     add = add-sub <'+'> mul-div
;     sub = add-sub <'-'> mul-div
;     <mul-div> = term | mul | div
;     mul = mul-div <'*'> term
;     div = mul-div <'/'> term
;     <term> = number | <'('> add-sub <')'>
;     number = #'[0-9]+'"))

(def expression
  (insta/parser
    "S = expr
    <expr> = NUMBER | VAR | ADD-SUB
    <ADD-SUB> = MUL-DIV | ADD | SUB
    <MUL-DIV> = NUMBER | MUL | DIV
    ADD = expr <'+'> MUL-DIV
    SUB = expr <'-'> MUL-DIV
    MUL = expr <'*'> NUMBER
    DIV = expr <'/'> NUMBER
    NUMBER = #'[0-9]'+
    VAR = #'[a-z]'+ #'[0-9]'*"))

(defn evaluate [eq x]
  (let [lookup-table {:x2 x :x x}]
    (insta/transform
      {:ADD +, :SUB -, :MUL *, :DIV /,
       :VAR (fn [& args] (->> args
                              (clojure.string/join nil)
                              keyword
                              lookup-table)),
       :NUMBER (fn [& args] (->> args
                                 (clojure.string/join nil)
                                 read-string)),
       :S identity} (expression eq))))

(defn get-points
  [eq x-range]
  (map #(vector % (evaluate eq %)) x-range))
