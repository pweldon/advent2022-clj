(ns advent2022.day12-1
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(def input-txt (slurp "input/day12.txt"))

(def example-input (slurp "input/day12-example.txt"))
(defn print-dist
  [d]
  (if (= d Integer/MAX_VALUE) "..∞.." (format "%5d" d)))

(defn print-state
  [state]
  (let [r (vals state)
        w (inc (apply max (map :x r)))
        h (inc (apply max (map :y r)))]
    (prn w h)
    (as->
     (reduce #(assoc %1 (+ (:x %2) (* w (:y %2))) (print-dist (:dist %2))) (vec (repeat (* w h) "..x..")) r) x
      ;;
      (partition-all w x)
      (map #(apply str "\n" %) x)

      (apply str x)
      (println x))))


(defn to-maps
  [input]
  (for [[y r] (zipmap (range) (str/split-lines input))]
    (for [[x c] (zipmap (range) r)]
      {:x x :y y :h c :type :path :visited false :dist Integer/MAX_VALUE})))

(defn add-start-end
  [cell]
  (condp = (:h cell)
    \S (assoc cell :h \a :type :start)
    \E (assoc cell :h \z :type :end :dist 0)
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


(defn next-node-to-visit
  [state]
  (let [unvisited-nodes (filter #(and (not (:visited %)) (< (:dist %) Integer/MAX_VALUE)) (vals state))]
    (if
     (empty? unvisited-nodes)
      nil
      (let [min-dist-node (apply min-key :dist unvisited-nodes)]
        [(:x min-dist-node) (:y min-dist-node)]))))

(defn update-neighbour-dist'
  [state x y x' y']
  (let [current-node (get state [x y])
        current-height (:h current-node)
        current-dist (:dist current-node)
        new-x (+ (:x current-node) x')
        new-y (+ (:y current-node) y')]
    (if-let [new-node (get state [new-x new-y])]
      (let [new-dist (+ current-dist 1)]
        ;;(prn new-node)
        (if (or
             (:visited new-node) ;; node has already been visited
             (< 1 (- current-height (:h new-node))) ;; height difference is too large
             (> new-dist (:dist new-node)) ;; path is not shorter
             )
          state ;; nothing to do return state unchanged
          ;; found a shorter path to new-node, update state
          (assoc state [new-x new-y] (assoc new-node :dist new-dist))))
      state)))

(defn update-neightbours
  [state x y]
  ;;(print-state state)
  (reduce #(apply update-neighbour-dist' %1 x y %2) state [[0 1] [0 -1] [1 0] [-1 0]]))

(defn dijkstra'
  [state]
  (if-let [next-node (next-node-to-visit state)]
    (let [[min-x min-y] next-node]
      (update (update-neightbours state min-x min-y) [min-x min-y] #(assoc % :visited true)))
    (reduced state)))

(defn dijkstra
  [state]
  (reduce (fn [s _] (dijkstra' s)) state (range)))

;; part 1


(defn puzzle-2
  [input]
  (let [state (parse-input input)
        d (dijkstra state)
        ds (filter #(= (:h %) 0) (vals d))
        shortest-d (apply min (map :dist ds))]
    (print-state d)
    shortest-d))


(comment
  (puzzle-2 input-txt)
  ;; => 528
  ;;
  (puzzle-2 example-input)
  ;; => 29
  )
