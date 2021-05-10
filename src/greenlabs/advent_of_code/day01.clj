(ns greenlabs.advent-of-code.day01 
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(-> (slurp (io/resource "day01.txt"))
    str/split-lines
    (read-string))