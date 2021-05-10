(ns greenlabs.advent-of-code.day01 
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))
;; part one
(->> (slurp (io/resource "day01.txt"))
    str/split-lines
     (map read-string)
     (apply +))

;; part two
(def input (->> "day01.txt"
                io/resource
                slurp
                str/split-lines
                (map read-string)))

(loop [i (cycle input)
       acc 0
       s #{0}]
  (if (seq i)
    (let [res (+ acc (first i))]
      (if (contains? s res)
        res
        (recur (next i) res (conj s res))))
    nil))