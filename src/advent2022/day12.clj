(ns advent2022.day12
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(def input-txt (slurp "input/day12.txt"))

(def example-input (slurp "input/day12-example.txt"))


(defn to-maps
  [input]
  (for [[y r] (zipmap (range) (str/split-lines input))]
    (for [[x c] (zipmap (range) r)]
      {:x x :y y :h c :type :path :visited false})))

(defn add-start-end
  [cell]
  (condp = (:h cell)
    \S (assoc cell :h \a :type :start)
    \E (assoc cell :h \z :type :end)
    cell))

(defn elevation-to-int
  [cell]
  (update cell :h #(- (int %) (int \a))))

(defn parse-input
  [input]
  (as-> input $
    (to-maps $)
    (flatten $)
    (map add-start-end $)
    (map elevation-to-int $)
    (map #(vector (vector (:x %) (:y %)) %) $)
    (into {} $)))

(defn visit'
  [state q n x' y']
  (let [current-cell (nth q n)
        current-height (:h current-cell)
        new-x (+ (:x current-cell) x')
        new-y (+ (:y current-cell) y')]
    (if-let [new-cell (get state [new-x new-y])]
      (if
       (and (>= 1 (- current-height (:h new-cell)))
            (empty? (filter #(and (= (:x %) new-x) (= (:y %) new-y)) q)))
        (list (assoc new-cell :n n))
        '())
      '())))

(defn visit
  [state end q n]
  ;;(prn n q state)
  (let [current-cell (nth q n)
        up (visit' state q n 0 1)
        down (visit' state q n 0 -1)
        left (visit' state q n -1 0)
        right (visit' state q n 1 0)]
    (if (= end (:type current-cell))
      q
      (recur state end (concat q up down left right) (inc n)))))

;; part 1


(defn puzzle-1
  [input]
  (let [state (parse-input input)
        start (filter #(= :start (:type %)) (vals state))]
    (visit state :end start 0)))

;; part 2

(defn puzzle-2
  [input]
  0)

(comment
  (puzzle-1 input-txt)
  ;; => 112815
  (puzzle-2 input-txt)
  ;; => 25738411485 
  (parse-input example-input)
  (map add-start-end (flatten (to-maps example-input)))
  (puzzle-1 example-input)
  (filter #(= :end (:type %)) (vals (parse-input example-input)))
  (apply max (map :x (vals (parse-input example-input))))
  (let [r (puzzle-1 example-input)
        w (apply max (map :x r))
        h (apply max (map :y r))]
    (prn w h)
    (as-> (reduce #(assoc %1 (+ (:x %2) (* w (:y %2))) (format "%4d" (:n %2))) (vec (repeat (* (inc w) (inc h)) ".")) r) x
      (partition w x))
    )

  ;;
  )
