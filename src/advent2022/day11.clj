(ns advent2022.day11
  (:require [clojure.string :as str]))

(def input-txt (slurp "input/day11.txt"))

(defn to-number
  [input]
  (if
   (re-matches #"-?[0-9]+" input)
    (bigint (parse-long input))
    input))


(defn init-state
  [relief-factor]
  {:monkeys [] :relief-factor relief-factor})

(defn parse-monkey-action
  [state [_ action-for str-throw-to]]
  (let [monkey (last (:monkeys state))
        action (keyword "action" action-for)]
    (->> (assoc monkey action (to-number str-throw-to))
         (assoc-in state [:monkeys (dec (count (:monkeys state)))]))))

(defn parse-monkey-test
  [state [_ str-divisble-by]]
  (assoc-in state [:monkeys (dec (count (:monkeys state))) :test-divisible-by]  (to-number str-divisble-by)))

(defn operation-fn
  [old operator value]
  (cond
    (= operator "*") (* old (if (= value "old") old value))
    (= operator "/") (/ old (if (= value "old") old value))
    (= operator "+") (+ old (if (= value "old") old value))
    (= operator "-") (* old (if (= value "old") old value))))


(defn parse-monkey-operation
  [state [_ operator str-number]]
  (let [monkey (last (:monkeys state))]
    (->> (assoc monkey :operation-operator operator :operation-value (to-number str-number))
         (assoc-in state [:monkeys (dec (count (:monkeys state)))]))))

(defn parse-monkey-items
  [state [_ str-items & _]]
  (assoc-in state [:monkeys (dec (count (:monkeys state))) :items] (vec (map to-number (str/split str-items #", ")))))

(defn parse-monkey-start
  [state [_ str-monkey-number]]
  (let [monkey-number (to-number str-monkey-number)]
    (assoc-in state [:monkeys monkey-number] {:id monkey-number :inspections (bigint 0)})))


(def parse-map
  [{:re #"Monkey (\d+):" :parser parse-monkey-start}
   {:re #"\s+Starting items: ((\s*\d+,?)+)" :parser parse-monkey-items}
   {:re #"\s+Operation: new = old (\+|-|\*|\/) (old|\d+)" :parser parse-monkey-operation}
   {:re #"\s+Test: divisible by (\d+)" :parser parse-monkey-test}
   {:re #"\s+If (true|false): throw to monkey (\d+)" :parser parse-monkey-action}])

(defn apply-matcher
  [s state matcher]
  (let [match (re-matches (:re matcher) s)
        parser (:parser matcher)]
    (if match
      (reduced (parser state match))
      state)))

(defn parse-line [state line]
  (reduce (partial apply-matcher line) state parse-map))

(defn lcm
  [xs]
  (let
   [gcd (fn gcd [a b] (if (= 0 b) a (recur b (mod a b))))
    lcm (fn lcm [a b] (/ (* a b) (gcd a b)))]
    (reduce lcm xs)))

(defn calc-scale-factor
  [state]
  (assoc state :scale-factor (lcm (map :test-divisible-by (:monkeys state)))))

(defn parse-input
  [input init-state]
  (let [lines (str/split-lines input)]
    (-> (reduce parse-line init-state lines)
        calc-scale-factor)))

(defn test-fn [worry divisor true-monkey false-monkey]
  (if (= 0 (mod worry divisor)) true-monkey false-monkey))

(defn do-monkey-turn-item
  [m state _]
  (let [monkey (nth (:monkeys state) m)
        items (:items monkey)
        item (first items)
        worry (operation-fn item (:operation-operator monkey) (:operation-value monkey))
        worry' (mod (quot worry (:relief-factor state)) (:scale-factor state))
        to-monkey (test-fn worry' (:test-divisible-by monkey) (:action/true monkey) (:action/false monkey))]
    (as-> state x
      (assoc-in x [:monkeys m :items] (vec (rest items)))
      (update-in x [:monkeys to-monkey :items] conj worry')
      (update-in x [:monkeys m :inspections] inc))))

(defn do-monkey-turn
  [state m]
  (reduce (partial do-monkey-turn-item m) state (range (count (get-in state [:monkeys m :items])))))

(defn do-round
  [state r]
  (reduce do-monkey-turn state (range (count (:monkeys state)))))

(defn go
  [input relief rounds]
  (as-> (reduce do-round (parse-input input (init-state relief)) (range rounds)) x
    (map :inspections (:monkeys x))
    (sort > x)
    (take 2 x)
    (apply * x)))

;; part 1


(defn puzzle-1
  [input]
  (go input 3 20))

;; part 2

(defn puzzle-2
  [input]
  (go input 1 1000))

(comment
  (puzzle-1 input-txt)
  ;; => 112815
  (puzzle-2 input-txt)
  ;; => 25738411485 
  )
