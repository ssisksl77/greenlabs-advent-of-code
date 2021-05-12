(ns greenlabs.advent-of-code.util
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn slurp-resource-lines [path]
  (let [lines (slurp (io/resource path))]
    (str/split-lines lines)))


(defn dup [f]
  (fn [col]
    (loop [xs col
           acc 0
           seen? #{}]
      (if (seq xs)
        (let [acc' (f acc (first xs))]
          (if (seen? acc')
            acc'
            (recur (next xs) acc' (conj seen? acc'))))))))
