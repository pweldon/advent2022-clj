(ns advent2022.day8-test
  (:require
   [advent2022.day8 :refer [puzzle-1 puzzle-2  visible-along? parse-input]]
   [clojure.test :refer [deftest is]]
   ))

(def example-input "30373\n25512\n65332\n33549\n35390")


(deftest test-visible-along?
  (is (= 
       true 
       (visible-along? 
        '(2 5 3 1 2) 1))))

(deftest parse-input-test
  (is (= 
       '((3 0 3 7 3) (2 5 5 1 2) (6 5 3 3 2) (3 3 5 4 9) (3 5 3 9 0))
       (parse-input example-input))))

(deftest puzzle-example-1
  (is (= 
       21 
       (puzzle-1 example-input))))

(deftest puzzle-example-2
  (is (= 
       8 
       (puzzle-2 example-input))))
