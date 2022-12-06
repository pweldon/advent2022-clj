(ns advent2022.day3
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))


(def input-txt (slurp "input/day3.txt"))

(def example-input
  "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(def char-value-map
  (map char
       (concat
        (range (int \a) (inc (int \z)))
        (range (int \A) (inc (int \Z))))))

(defn char-value
  [c]
  (inc (.indexOf char-value-map c)))

(defn puzzle-1
  [input]
  (->> input
       str/split-lines
       (map str/trim)
       (filter #(not (str/blank? %)))
       (map #(partition (/ (count %) 2) %))
       (map #(map set %))
       (map #(apply set/intersection %))
       (map first)
       (map char-value)
       (apply +)))

(deftest puzzle-example
  (is (= 157 (puzzle-1 example-input))))

(comment
  (puzzle-1 input-txt)
  ;; => 7848
  )

(defn puzzle-2
  [input]
  (->> input
       str/split-lines
       (map str/trim)
       (filter #(not (str/blank? %)))
       (map set)
       (partition 3)
       (map #(apply set/intersection %))
       (map first)
       (map char-value)
       (apply +)))

(comment
  (puzzle-2 input-txt)
  ;; => 2616
  )

(defn puzzle-3
  [input]
  (->> input
       str/split-lines
       (map str/trim)
       (filter #(not (str/blank? %)))
       (map (fn [l] (->> l
                        (#(partition (/ (count %) 2) %))
                        (#(map set %))
                        (apply set/intersection)
                        first
                        char-value)))
       (apply +)))

(puzzle-3 input-txt)
