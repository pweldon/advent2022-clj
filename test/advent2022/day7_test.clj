(ns advent2022.day7-test
  (:require
    [advent2022.day7 :refer [puzzle-1 puzzle-2 parse-dir parse-dirs get-sizes]]
    [clojure.test :refer [deftest is]]
    [clojure.string :as str]))

(def example-input
  "
$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
  ")

(deftest parse-dir-test
  (is (= {:name "a" :size 10} (parse-dir "a abc\n det\n 1 2 3 4"))))

(deftest parse-dirs-test
  (is (= '({:name "/"
            :size 23352670}
           {:name "a"
            :size 94269}
           {:name "e"
            :size 584}
           {:name ".."
            :size 0}
           {:name ".."
            :size 0}
           {:name "d"
            :size 24933642}) (parse-dirs example-input))))

(deftest get-sizes-test
  (is (= 48381165 ((get-sizes (parse-dirs example-input)) "/")))
  )

(deftest puzzle-example-1
  (is (= 95437 (puzzle-1 example-input)))
  )

(deftest puzzle-example-2
  (is (= 24933642 (puzzle-2 example-input)))
  )
