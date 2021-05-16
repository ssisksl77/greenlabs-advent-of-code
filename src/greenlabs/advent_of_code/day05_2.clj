(ns greenlabs.advent-of-code.day05-2
  (:require [clojure.java.io :as io]))

(defn is-same-unit?
  [u1 u2]
  (= 32 (Math/abs (- (int u1) (int u2)))))

(def demo-input "dabAcCaCBAcCcaDA")
(def input (slurp (io/resource "day05.txt")))


(defn part-one [input]
  (count (reduce (fn [res c]
                   (if (and (seq res)
                            (is-same-unit? c (peek res)))
                     (pop res)
                     (conj res c)))
                 '()
                 input)))

(time (part-one input))

(defn react-poly [data res]
  (if (and (seq res)
           (is-same-unit? (first data)
                          (peek res)))
    [(rest data) (pop res)]
    [(rest data) (conj res (first data))]))


(defn part-two []
  (let [alpha-range   (map (fn [c]  #{(char c) (char (+ c 32))})
                           (range (int \A) (inc (int \Z))))
        removed-strs (map (fn [x] (apply str (remove x input))) alpha-range)]
    (time (apply min (pmap part-one
                           removed-strs)))))

(part-two) ;; 5412
