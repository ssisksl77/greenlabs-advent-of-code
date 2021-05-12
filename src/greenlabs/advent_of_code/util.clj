(ns greenlabs.advent-of-code.util
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn slurp-resource-lines [path]
  (let [lines (slurp (io/resource path))]
    (str/split-lines lines)))


(defn dup [f col]
  (loop [xs col
         acc 0
         seen? #{}]
    (if (seq xs)
      (let [acc' (f acc (first xs))]
        (if (seen? acc')
          acc'
          (recur (next xs) acc' (conj seen? acc')))))))


(defn dup-reduce [col]
  (->> col
       (reduce (fn [acc e]
                 (if (seq (:dup acc))
                   (reduced acc)
                   (if ((:seen? acc) e)
                     (update-in acc [:dup] conj e)
                     (update-in acc [:seen?] conj e)))
                 )
               {:seen? #{} :dup #{}})
       :dup
       first
       ))
