(ns greenlabs.advent-of-code.2018.day08
  (:require [clojure.string :as str]))

;; add up all metadatas
(defn parse [input]
  (str/split input #" "))

(def demo-input (->> (parse "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2")
                     (map #(Integer/parseInt %))))


(defn part-one [input]
  (loop [[child-num meta-num & rest'] input
         meta-data []]
    (prn (str "I have " child-num " childs. and " meta-num " meta data."))
    (if-let [[my-meta-data rest'] (if (and (= 0 child-num )
                                     (< 0 meta-num))
                              (split-at meta-num rest')
                              nil)]
      (do (prn ["  meta-data : " my-meta-data])
          (recur rest' (concat meta-data my-meta-data)))
      (if (seq rest')
        (recur rest' meta-data)
        meta-data))
    ))

(part-one demo-input)

(if (= 138 (part-one demo-input))
  "demo-input success"
  "demo-input fail")
