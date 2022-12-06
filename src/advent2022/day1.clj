(ns advent2022.day1
  (:require clojure.string
            [clojure.test :refer [deftest is run-tests]]))


(def input-txt (slurp "input/day1.txt"))

(defn group-by-elf
  [txt]
  (map #(clojure.string/split-lines %) (clojure.string/split txt #"\n\n")))

(deftest group-by-elf-test
  (is (= 
       (group-by-elf "1\n\n2\n3\n\n4")
       `(["1"] ["2" "3"] ["4"]))))

(defn to-int
  [group]
  (map (fn [s] (Integer/parseInt s)) group))

(deftest to-int-test
  (is (=
       (to-int ["2" "3"])
       `(2 3))))

(defn sum-group
  [group]
  (reduce + group))

(deftest sum-group-test
  (is (= 
       (sum-group `(2 3))
       5)))

(defn answer1
  [input]
  (->> input
       group-by-elf
       (map to-int)
       (map sum-group)
       (apply max)))

(defn answer2
  [input]
  (->> input
       group-by-elf
       (map to-int)
       (map sum-group)
       sort
       reverse
       (take 3)
       sum-group))

(comment  
  (run-tests)
  (answer1 input-txt) 
  ;; => 71124
  
  (answer2 input-txt) 
  ;; => 204639
  )
