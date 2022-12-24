(ns advent2022.day13
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(def input-txt (slurp "input/day13.txt"))

(def example-input (slurp "input/day13-example.txt"))

(defn compare-packets'
  [left right]
  ;;(prn "l:" left)
  ;;(prn "r:" right)
  (cond
    (and (int? left) (int? right)) (cond
                                     (> left right) false
                                     (< left right) true
                                     :else :continue)
    (and (vector? left) (int? right)) (recur left [right])
    (and (int? left) (vector? right)) (recur [left] right)
    (and (empty? left) (empty? right)) :continue
    (empty? left) true
    (empty? right) false
    :else (condp = (compare-packets' (first left) (first right))
            :continue (recur (vec (rest left)) (vec (rest right)))
            true true
            false false)))

(defn compare-packets
  [left right]
  (let [r (compare-packets' left right)]
    ;;(prn "==> " r)
    r))

(defn parse-input
  [input]
  (as-> input $
    (str/split-lines $)
    (filter not-empty $)
    (map read-string $)))

;; part 1


(defn puzzle-1
  [input]
  (as-> input $
    (parse-input $)
    (partition-all 2 $)
    (map-indexed (fn [idx pair] [(inc idx) pair (apply compare-packets pair)]) $)
    (filter (comp true? #(nth % 2)) $)
    (map first $)
    (apply + $)
    ;;
    ))

;; part 2

(defn puzzle-2
  [input]
  (let [div-2 [[2]]
        div-6 [[6]]]
    (as-> input $
      (parse-input $)
      (conj $ div-2 div-6)
      (sort-by identity compare-packets $)
      (map-indexed (fn [idx p] [(inc idx) p]) $)
      (filter #(condp = (second %) div-2 true div-6 true false) $)
      (map first $)
      (apply * $))))

(comment
  (puzzle-1 example-input)
  ;; => 13
  (puzzle-1 input-txt)
  ;; => 5808
  (puzzle-2 example-input)
  ;; => 140 
  (puzzle-2 input-txt)
  ;; => 27713 
  ;; 
  )


