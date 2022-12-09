(ns advent2022.day7
  (:require [clojure.string :as str]))

(def input-txt (slurp "input/day7.txt"))

(defn to-number
  [input]
  (if
    (re-matches #"[0-9]+" input)
    (parse-long input)
    0))

(defn parse-dir
  [input]
  (let [s (str/split input #"[\ \n]")
        p (first s)
        size (apply + (map to-number s))]
    {:name p :size size}))

(defn parse-dirs
  [input]
  (->> (str/split input #"\$ cd ")
       (map parse-dir)
       (filter #(not= nil (:name %)))
       ))

(defn path-to-str
  [path]
  (str "/" (str/join "/" (rest path))))

(defn emit
  [path size]
  (for [n (range (count path))]
    (list (path-to-str (drop-last n path)) size)))

(defn process-items
  [state item]
  (let
    [new-path' (cons (:name item) (:path state))
     new-path (reverse new-path')
     new-sizes (reduce (fn [m x] (update m (first x) #(+ (or % 0) (second x)))) (:sizes state) (emit new-path (:size item)))
     ]
    (if
      (= ".." (:name item))
      (update state :path #(rest %))
      (assoc state :path new-path' :sizes new-sizes))))

(defn get-sizes
  [input]
  (:sizes (reduce process-items {:path () :sizes {}} input)))



(defn puzzle-1
  [input]
  (apply + (map second (filter #(> 100000 (val %)) (get-sizes (parse-dirs input))))))

(defn puzzle-2
  [input]
  (let [sizes (get-sizes (parse-dirs input))
        sorted-sizes (sort-by second sizes)
        total-size 70000000
        needed-free-space 30000000
        current-free-space (- total-size (second (last sorted-sizes)))
        need-to-delete (- needed-free-space current-free-space)
        dir-to-delete (drop-while #(> need-to-delete (second %)) sorted-sizes)
        ]
    (second (first dir-to-delete))))

(comment
  (puzzle-1 input-txt)
  ;; => 1086293
  (puzzle-2 input-txt)
  ;; => 366028
  )
