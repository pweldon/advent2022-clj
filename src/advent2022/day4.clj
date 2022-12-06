(ns advent2022.day4
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))


(def input-txt (slurp "input/day4.txt"))

(def example-input
  "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(defn is-contained?
  [[a b c d]]
  (or
   (and (<= a c) (<= d b))
   (and (<= c a) (<= b d))))

(deftest is-contained-test
  (is (= true (is-contained? '(1 10 2 9)))))

(defn overlaps?
  [[a b c d]]
  (and (<= a d) (>= b c)))

(defn puzzle-1
  [input]
  (->> input
       (#(str/split % #"[\n,-]"))
       (map #(Integer/parseInt %))
       (partition 4)
       (filter is-contained?)
       count))

(deftest puzzle-example
  (is (= 2 (puzzle-1 example-input))))

(comment
  (puzzle-1 input-txt)
  ;; => 453
  )

(defn puzzle-2
  [input]
  (->> input
       (#(str/split % #"[\n,-]"))
       (map #(Integer/parseInt %))
       (partition 4)
       (filter overlaps?)
       count))

(comment
  (puzzle-2 input-txt)
  ;; => 919
  )
