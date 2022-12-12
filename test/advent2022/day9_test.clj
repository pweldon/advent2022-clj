(ns advent2022.day9-test
  (:require
   [advent2022.day9 :refer [puzzle-1 puzzle-2 parse-input move-head init-state]]
   [clojure.test :refer [deftest is]]
   [clojure.string :as str]))

(def example-input "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(def example-input-2 "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")

(deftest parse-input-test
  (is (=
       '("R"
         "R"
         "R"
         "R"
         "U"
         "U"
         "U"
         "U"
         "L"
         "L"
         "L"
         "D"
         "R"
         "R"
         "R"
         "R"
         "D"
         "L"
         "L"
         "L"
         "L"
         "L"
         "R"
         "R")
       (parse-input example-input))))

(deftest move-head-test
  (let [state (init-state 2)]
    (is (= {:x 0 :y 1}  (get-in (move-head state "U") [:knots 0])))
    (is (= {:x 0 :y -1} (get-in (move-head state "D") [:knots 0])))
    (is (= {:x -1 :y 0} (get-in (move-head state "L") [:knots 0])))
    (is (= {:x 1 :y 0} (get-in (move-head state "R") [:knots 0])))))

(deftest puzzle-example-1
  (is (=
       13
       (puzzle-1 example-input))))

(deftest puzzle-example-2
  (is (=
       36
       (puzzle-2 example-input-2))))
