(ns greenlabs.advent-of-code.2018.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; https://adventofcode.com/2018/day/10
(->> (slurp (io/resource "2018/day10_demo.txt"))
     (str/split-lines))

(defn make-point [str]
  (let [[id x y dx dy] (->> str
                            (re-find #"position=<[\s]?(-?\d+),\s[\s]?(-?\d+)> velocity=<[\s]?(-?\d+),\s[\s]?(-?\d+)>")
                            rest)]
    {:id id :x x :y y :dx dx :dy dy}))

(defn parse [s]
  (->> s
       (str/split-lines)
       (map make-point)))

(parse (slurp (io/resource "2018/day10_demo.txt")))
