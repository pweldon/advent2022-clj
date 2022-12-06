(ns advent2022.day5-test
  (:require
   [advent2022.day5 :refer [do-move parse-init-stacks parse-input puzzle-1 puzzle-2]]
   [clojure.test :refer [deftest is]]))

(def example-input
  "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")


(def stacks (first (parse-input example-input)))

(deftest parse-init-stacks-test
  (is (= {1 '(\N \Z), 2 '(\D \C \M), 3 '(\P)} (parse-init-stacks stacks))))

(deftest puzzle-example
  (is (= "CMZ" (puzzle-1 example-input))))

(deftest puzzle-example-2
  (is (= "MCD" (puzzle-2 example-input))))

(deftest do-move-test
  (is (= {1 '() 2 '(\A)} (do-move {1 '(\A) 2 '()} [1 1 2]))))