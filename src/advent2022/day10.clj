(ns advent2022.day10
  (:require [clojure.string :as str]))

(defn parse-input
  [input]
  (as-> input x
    (str/split-lines x)
    (map #(str/split % #"[\ ]") x)
    (mapcat #(repeat (parse-long (second %)) (first %)) x)))

(def input-txt (slurp "input/day10.txt"))

(defn to-number
  [input]
  (if
   (re-matches #"-?[0-9]+" input)
    (parse-long input)
    input))

(defn parse-input
  [input]
  (->> (str/split-lines input)
       (map #(map to-number (str/split % #"[\ ]")))))

(defn print-state
  [state msg]
  (prn msg)
  (prn {:x (:x state) :pixel (:pixel state)})
  state)

(defn init-state
  [instructions]
  {:cycle 0
   :x 1
   :busy 0
   :running nil
   :instructions instructions
   :xs [1]})

(defn do-addx
  [state]
  (-> state
      (update :x #(+ % (second (:running state))))))

(defn do-noop
  [state]
  state)

(defn complete-instruction
  [state]
  (cond
    (= "noop" (first (:running state))) (do-noop state)
    (= "addx" (first (:running state))) (do-addx state)))

(def instruction-cycles
  {"addx" 2 "noop" 1})

(defn load-instruction
  [state]
  (if (= 0 (:busy state))
    (let [instructions (:instructions state)
          instruction (first instructions)
          op-code (first instruction)
          op-code-cycles (get instruction-cycles op-code)]
      (assoc state
             :running instruction
             :instructions (rest instructions)
             :busy op-code-cycles))
    state))


(defn do-instruction
  [state]
  (-> state
      (update :busy dec)
      (#(if (= 0 (:busy %)) (complete-instruction %) %))))

(defn update-cycle
  [state]
  (update state :cycle #(+ 1 %)))

(defn init
  [state]
  (load-instruction state))

(defn record-x
  [state]
  (-> state 
      
      (update :xs #(conj % (:x state)))
      (#(assoc % :pixel (mod (:cycle %) 40)))))

(defn halt
  [state]
  (if (and (= 0 (:busy state)) (empty? (:instructions state))) (reduced state) state))

(defn cycle'
  [state cycle]
  (-> state
      load-instruction
      do-instruction
      record-x
      update-cycle
      (print-state (str cycle))
      halt))

(defn run
  [state]
  (-> state
      init
      (#(reduce cycle' % (range)))))

;; part 1

(defn puzzle-1
  [input]
  (->> (run (init-state (parse-input input)))
       (:xs)
       (drop 19)
       (partition 1 40)
       (map first)
       (map-indexed #(* (+ 20 (* %1 40)) %2))
       (apply +)))


;; part 2

(defn draw-sprite
  [cycle x]
  (let [sprite-min (dec x)
        sprite-max (inc x)
        draw (<= sprite-min (mod cycle 40) sprite-max)]
    (if draw "#" ".")))

(defn puzzle-2
  [input]
  (->> (run (init-state (parse-input input)))
       (:xs)
       (map-indexed draw-sprite)
       (partition 40)
       (map #(apply str %))))

(comment
  (puzzle-1 input-txt)
  ;; => 13440
  (puzzle-2 input-txt)
  ;; => PBZGRAZA
  )
