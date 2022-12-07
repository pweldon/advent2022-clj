(ns advent2022.day6-test
  (:require
   [advent2022.day6 :refer [puzzle-1]]
   [clojure.test :refer [deftest is]]))


(deftest puzzle-example-1
  (is (= 7 (puzzle-1  "mjqjpqmgbljsphdztnvjfqwrcgsmlb" 4)))
  (is (= 5 (puzzle-1  "bvwbjplbgvbhsrlpgdmjqwftvncz" 4)))
  (is (= 6 (puzzle-1  "nppdvjthqldpwncqszvftbrmjlhg" 4)))
  (is (= 10 (puzzle-1   "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 4)))
  (is (= 11 (puzzle-1   "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 4)))
  )

(deftest puzzle-example-2
  (is (= 19 (puzzle-1  "mjqjpqmgbljsphdztnvjfqwrcgsmlb" 14)))
  (is (= 23 (puzzle-1  "bvwbjplbgvbhsrlpgdmjqwftvncz" 14)))
  (is (= 23 (puzzle-1  "nppdvjthqldpwncqszvftbrmjlhg" 14)))
  (is (= 29 (puzzle-1   "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 14)))
  (is (= 26 (puzzle-1   "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 14)))
  )

