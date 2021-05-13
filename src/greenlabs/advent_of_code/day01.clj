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
(util/dup-one
 (reductions + (cycle (read-input (io/resource "day01.txt")))))

#_(util/dup-one2
 (reductions + (cycle (read-input (io/resource "day01.txt")))))
