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
    <VAR-NUM> = NUMBER | VAR | <'('> ADD-SUB <')'>
    MAYBESPACE = ' ' | EPSILON
    NUMBER = #'[0-9]'+
    VAR = #'[a-z]'+"))

(defn evaluate [rhs x]
  (let [lookup-table {:x x :y x}]
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

(defn rectify-range [range]
  (map #(->> %
             float
             (format "%.1f")
             Double/parseDouble) range))

(defn create-point [sym val1 val2]
  {sym val1 (sym {:x :y :y :x}) val2})

(defn remove-spaces [text]
  (->> text
       (remove (partial = \space))
       (apply str)))

(defn get-points
  [eq x-range]
  (let [sides (clojure.string/split eq #"=")
        rhs (last sides)
        lhs (first sides)]
    (map #(create-point (keyword lhs) (evaluate rhs %) %) (rectify-range x-range))))
