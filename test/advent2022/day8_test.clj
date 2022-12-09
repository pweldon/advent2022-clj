(ns advent2022.day8-test
  (:require
    [advent2022.day8 :refer [puzzle-1 puzzle-2 as-score visible-along? parse-input]]
    [clojure.test :refer [deftest is]]
    [clojure.string :as str]))

(def example-input "30373\n25512\n65332\n33549\n35390")



  (deftest test-visible-along?
    (is (= true (visible-along? '(2 5 3 1 2) 1))))

(deftest parse-input-test
  (is (= '() (parse-input example-input))))

(deftest puzzle-example-1
  (is (= -1 (puzzle-1 example-input)))
  )

(deftest puzzle-example-2
  (is (= -1 (puzzle-2 example-input)))
  )
