(ns greenlabs.advent-of-code.day01 
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-input [path]
  (->> path 
       slurp
       str/split-lines
       (map read-string)))

;; part one
(->> (io/resource "day01.txt")
     read-input
     (apply +))

;; part two
(loop [i (cycle (read-input (io/resource "day01.txt")))
       acc 0
       s #{0}]
  (if (seq i)
    (let [res (+ acc (first i))]
      (if (contains? s res)
        res
        (recur (next i) res (conj s res))))
    nil))