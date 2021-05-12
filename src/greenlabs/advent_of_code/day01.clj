(ns greenlabs.advent-of-code.day01
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [greenlabs.advent-of-code.util :as util]))

(defn read-input [path]
  (let [lines (-> path slurp str/split-lines)]
    (map read-string lines)))

;; part one
(defn part-one [ints]
  (apply + ints))

(part-one (->> (io/resource "day01.txt")
               read-input))

;; PPAP
; Parse
; Processing (Map)
; Aggregate (Reduce)
; Print (Js.log)

;; part two
(util/dup-reduce
 (reductions + (cycle (read-input (io/resource "day01.txt")))))

(comment
  (defn part-two [ints]
    (loop [xs (cycle ints)
           acc 0
           seen? #{0}]
      (if (seq xs)
        (let [acc' (+ acc (first xs))]
          (if (seen? acc')
            acc'
            (recur (next xs) acc' (conj seen? acc'))))
        nil)))

  (part-two (read-input (io/resource "day01.txt"))))
