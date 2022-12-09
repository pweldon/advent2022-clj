(ns advent2022.day8
  (:require [clojure.string :as str]))

(defn parse-input
  [input]
  (let [
        lines (str/split-lines input)
        trees (map #(map (comp parse-long str) (seq %)) lines)
        ]
    trees))

(defn x-size
  [m]
  (count (first m)))

(defn y-size
  [m]
  (count m))

(defn xs-for-y
  [m y]
  (nth m y))

(defn ys-for-x
  [m x]
  (map #(nth % x) m))

(defn visible-along?
  [axis pos]
  (let [before-max (apply max 0 (take pos axis))
        after-max (apply max 0 (drop (inc pos) axis))
        height (nth axis pos)
        ]
    (or (= pos 0) (= pos (dec (count axis))) (> height before-max) (> height after-max))))

(defn visible-along-x?
  [m x y]
  (visible-along? (xs-for-y m y) x))

(defn visible-along-y?
  [m x y]
  (visible-along? (ys-for-x m x) y))

(defn visible?
  [m x y]
  (or
    (visible-along-x? m x y)
    (visible-along-y? m x y)))

(defn as-visible
  [input]
  (let [trees (parse-input input)]
    (for [x (range (x-size trees))
          y (range (y-size trees))]
      (visible? trees x y))))


(defn visible?
  [m x y]
  (or
    (visible-along-x? m x y)
    (visible-along-y? m x y)))


;; part 2

(defn can-see
  [h xs]
  (reduce (fn [acc x]
            (if
              (< x h)
              (inc acc)
              (reduced (inc acc))))
          0
          xs))

(defn score-along
  [axis pos]
  (let [height (nth axis pos)
        before-score (can-see height (reverse (take pos axis)))
        after-score (can-see height (drop (inc pos) axis))
        ]
    (* before-score after-score)))

(defn score-along-x
  [m x y]
  (score-along (xs-for-y m y) x))

(defn score-along-y
  [m x y]
  (score-along (ys-for-x m x) y))

(defn score
  [m x y]
  (*
    (score-along-x m x y)
    (score-along-y m x y)))

(defn as-score
  [input]
  (let [trees (parse-input input)]
    (for [x (range (x-size trees))
          y (range (y-size trees))]
      (score trees x y))))


;;

(def input-txt (slurp "input/day8.txt"))

(defn puzzle-1
  [input]
  (count (filter true? (as-visible input))))

(defn puzzle-2
  [input]
  (apply max (as-score input)))

(comment
  (puzzle-1 input-txt)
  ;; => 1672
  (puzzle-2 input-txt)
  ;; => 327180
  )
