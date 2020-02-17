(ns math-exp-evaluator.core
  (:require [instaparse.core :as insta]))

(def expression
  (insta/parser
    "S = <MAYBESPACE> expr
    <expr> = VAR-NUM | ADD-SUB
    <ADD-SUB> = MUL-DIV | ADD | SUB
    <MUL-DIV> = TRIGOFUNC | POWER | MUL | DIV
    <TRIGOFUNC> = VAR-NUM | SIN | COS | TAN
    ADD = expr <MAYBESPACE '+' MAYBESPACE> MUL-DIV
    SUB = expr <MAYBESPACE '-' MAYBESPACE> MUL-DIV | <'-'> MUL-DIV
    MUL = expr <MAYBESPACE '*' MAYBESPACE> MUL-DIV | NUMBER VAR
    DIV = expr <MAYBESPACE '/' MAYBESPACE> MUL-DIV
    POWER = expr <MAYBESPACE '^' MAYBESPACE> VAR-NUM
    SIN = <'sin'> VAR-NUM
    COS = <'cos'> VAR-NUM
    TAN = <'tan'> VAR-NUM
    <VAR-NUM> = NUMBER | VAR
    MAYBESPACE = ' ' | EPSILON
    NUMBER = #'[0-9]'+
    VAR = #'[a-z]'+"))

(defn evaluate [eq x]
  (let [lookup-table {:x x}
        rhs (last (clojure.string/split eq #"="))]
    (insta/transform
      {:ADD    +, :SUB -, :MUL *, :DIV /,
       :POWER  (fn [x p] (Math/pow x p)),
       :SIN    (fn [x] (Math/sin x)),
       :COS    (fn [x] (Math/cos x)),
       :TAN    (fn [x] (Math/tan x)),
       :VAR    (fn [& args] (->> args
                                 (apply str)
                                 keyword
                                 lookup-table)),
       :NUMBER (fn [& args] (->> args
                                 (apply str)
                                 read-string)),
       :S      identity} (expression rhs))))

(defn get-points
  [eq x-range]
  (map #(vector % (evaluate eq %)) x-range))
