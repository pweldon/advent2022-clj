(ns advent2022.day11-test
  (:require [advent2022.day11 :refer [puzzle-1 puzzle-2]]
            [clojure.test :refer [deftest is]]
            [clojure.string :as str]))

(def example-input (slurp "input/day11-example.txt"))

(deftest puzzle-example-1
  (is (=
       10605
       (puzzle-1 example-input))))


(comment
  (puzzle-1 example-input)
  ;; 10605
  (puzzle-2 example-input)
  )