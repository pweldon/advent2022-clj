(ns advent2022.day5
  (:require [clojure.string :as str]))

(def input-txt (slurp "input/day5.txt"))

(defn parse-input
  [input]
  (->> input
       (#(str/split % #"\n\n"))
       (map str/split-lines)))

(defn is-letter?
  [c]
  (re-matches #"[A-Z]" (str c)))

(defn is-digits?
  [c]
  (re-matches #"[0-9]+" c))

(defn only-letters
  [s]
  (map #(filter is-letter? %) s))

(defn parse-init-stacks
  [input]
  (->> input
       butlast
       (map #(partition-all 4 %))
       (map only-letters)
       (apply mapv vector)
       (map #(apply concat (filter (complement empty?) %)))
       (map-indexed (fn [i s] [(inc i) s]))
       (into {})))


(defn parse-move
  [input]
  (->> (str/split input #" ")
       (filter is-digits?)
       (map #(Integer/parseInt %))))

(defn parse-moves
  [input]
  (map parse-move input))

(defn do-move
  [stacks [num from to]]
  (let [[from_crates from_tail] (split-at num (stacks from))
        to_stack (apply conj (stacks to) from_crates)
        new-stacks (assoc stacks from from_tail to to_stack)]
    new-stacks))

(defn stack-tops
  [stacks]
  (->> (for [i (range 1 (inc (count stacks)))]
         (first (stacks i)))
       (apply str)))

(defn puzzle
  [input mover]
  (let [[init-stacks-input moves-input] (parse-input input)
        stacks (parse-init-stacks init-stacks-input)
        moves (parse-moves moves-input)
        final-stacks (reduce mover stacks moves)
        answer (stack-tops final-stacks)]
    answer))

(defn puzzle-1
  [input]
  (puzzle input do-move))


(defn do-move-2
  [stacks [num from to]]
  (let [[from_crates from_tail] (split-at num (stacks from))
        from_crates_r (reverse from_crates)
        to_stack (apply conj (stacks to) from_crates_r)]
    (assoc stacks from from_tail to to_stack)))


(defn puzzle-2
  [input]
  (puzzle input do-move-2))

(comment
  (puzzle-1 input-txt)
  ;; => "FRDSQRRCD"
  (puzzle-2 input-txt)
  ;; => "HRFTQVWNN"
  )
