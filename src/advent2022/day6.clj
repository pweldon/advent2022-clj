(ns advent2022.day6
  (:require [clojure.string :as str]))

(def input-txt (slurp "input/day6.txt"))

(defn puzzle-1
  [input marker-len]
  (let [s (str/trim input)
        ps (partition marker-len 1 s)
        not-unique (take-while #(< (count (set %)) marker-len) ps)
        answer (+ (count not-unique) marker-len)]
    answer))

(comment
  (puzzle-1 input-txt 4)
  ;; => 1625
  (puzzle-1 input-txt 14)
  ;; => 2250
  )
