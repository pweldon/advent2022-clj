(ns advent2022.day9
  (:require [clojure.string :as str]))

(defn parse-input
  [input]
  (as-> input x
    (str/split-lines x)
    (map #(str/split % #"[\ ]") x)
    (mapcat #(repeat (parse-long (second %)) (first %)) x)))

(def input-txt (slurp "input/day9.txt"))

(defn move-point
  [move point]
  (cond
    (= move "U") (update point :y inc)
    (= move "D") (update point :y dec)
    (= move "L") (update point :x dec)
    (= move "R") (update point :x inc)))

(defn init-state
  [n]
  {:knots (vec (repeat n {:x 0 :y 0}))
   :tail-set #{{:x 0 :y 0}}})

(defn move-head
  [state move]
  (update-in state [:knots 0] #(move-point move %)))

(defn max-distance
  [h t]
  (max (abs (- (:x h) (:x t)))
       (abs (- (:y h) (:y t)))))

(defn update-tail-set
  [state]
  (let [tail (last (:knots state))
        tail-set (:tail-set state)
        new-tail-set (conj tail-set tail)]
    (assoc state :tail-set new-tail-set)))

(defn move-tail'
  [state n axis]
  (let [head (get-in state [:knots (dec n) axis])
        tail (get-in state [:knots n axis])
        dir (compare head tail)
        new-tail (+ tail dir)]
    (update-in state [:knots n axis] (constantly new-tail))))

(defn move-tail
  [state n]
  (do
    (cond
      (> 2 (max-distance (get-in state [:knots (dec n)]) (get-in state [:knots n]))) state
      (= (get-in state [:knots (dec n) :x]) (get-in state [:knots n :x]) (move-tail' state n :x))
      (= (get-in state [:knots (dec n) :y]) (get-in state [:knots n :y]) (move-tail' state n :y))
      :else (-> state
                (move-tail' n :x)
                (move-tail' n :y)))))

(defn print-state [s]
  (let [h [(get-in s [:head :x]) (get-in s [:head :y])]
        t [(get-in s [:tail :x]) (get-in s [:tail :y])]]
    (println
     (str/join "\n"
               (for [y (reverse (range 6))]
                 (str/join " "
                           (for [x (range 6)]
                             (cond
                               (= [x y] h)     "H"
                               (= [x y] t)     "T"
                               (= [x y] [0 0]) "s"
                               :else           "."))))))))

(defn do-move
  [state move]
  (-> state
        (move-head move)
        (#(reduce (fn [s n] (move-tail s n)) % (range 1 (count (:knots state)))))
        (update-tail-set)))

(defn do-moves
  [state moves]
  (reduce do-move state moves))

;; part 1

(defn puzzle-1
  [input]
  (as-> input x
    (parse-input x)
    (do-moves (init-state 2) x)
    (:tail-set x)
    (count x)))

(defn puzzle-2
  [input]
  (as-> input x
    (parse-input x)
    (do-moves (init-state 10) x)
    (:tail-set x)
    (count x)))

(comment
  (puzzle-1 input-txt)
  ;; => 6018
  (puzzle-2 input-txt)
  ;; => 2619
  )
