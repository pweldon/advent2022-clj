(ns advent2022.day2
  (:require
   [clojure.test :refer [deftest is]]
   [clojure.string :as str]))

(def input-txt (slurp "input/day2.txt"))

(def score-matrix-1
  {["R" "R"] (+ 1 3)
   ["P" "R"] (+ 1 0)
   ["S" "R"] (+ 1 6)
   ["R" "P"] (+ 2 6)
   ["P" "P"] (+ 2 3)
   ["S" "P"] (+ 2 0)
   ["R" "S"] (+ 3 0)
   ["P" "S"] (+ 3 6)
   ["S" "S"] (+ 3 3)})

(def shape-map
  {"X" "R" "Y" "P" "Z" "S" "A" "R" "B" "P" "C" "S"})

(defn parse-move
  [line]
  (let [move (clojure.string/split line #" ")]
    (map shape-map move)))

(deftest parse-move-test
  (is (= `("R" "R") (parse-move "A X"))))

(defn score-move
  [score-matrix move]
  (score-matrix move))

(defn parse
  [input]
  (map parse-move (clojure.string/split-lines input)))

(deftest parse-test
  (is (= `(["R" "R"] ["P" "P"] ["S" "S"]) (parse "A X\nB Y\nC Z"))))

(defn score
  [score-matrix input]
  (->> (parse input)
       (map #(score-move score-matrix %))
       (apply +)))

(def example-input
  "A Y\nB X\nC Z")

(deftest example-test
  (is (= (score score-matrix-1 example-input) 15)))

(def score-matrix-2
  {["R" "R"] (+ 3 0)
   ["P" "R"] (+ 1 0)
   ["S" "R"] (+ 2 0)
   ["R" "P"] (+ 1 3)
   ["P" "P"] (+ 2 3)
   ["S" "P"] (+ 3 3)
   ["R" "S"] (+ 2 6)
   ["P" "S"] (+ 3 6)
   ["S" "S"] (+ 1 6)})


(comment
  (score score-matrix-1 input-txt)
  ;; => 15632
  (score score-matrix-2 input-txt)
  ;; => 14416
  )
