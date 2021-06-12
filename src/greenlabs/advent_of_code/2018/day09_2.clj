(ns greenlabs.advent-of-code.2018.day09-2)

(def arr (java.util.LinkedList.))
(doseq [i (range 20)]
  (.add arr i))
(java.util.Collections/rotate arr 6)
arr
(.removeLast arr)
(.add arr 100)
(.size arr)
(.get arr 30)

(defn rm [^Integer i]
  (.remove arr i))
(rm 2)
(defn solve
  "p - players
   n - number of marbles"
  [^long p ^long n]
  (loop [i 0
         prev-idx 0
         col []
         score {}]
    (if (= i n)
      score
      (let [cnt (count col)]
        (cond
          (empty? col)
          (recur 1 0 [0] {})

          (= cnt 1)
          (recur 2 1 [0 1] {})

          (= cnt 2)
          (recur 3 1 [0 2 1] {})

          (= 0 (mod i 23))
          (let [pivot (mod (+ cnt (- prev-idx 7)) cnt)
                removed-number (nth col pivot)
                scored-player (mod i p)]
            (recur (inc i)
                   pivot
                   (into (subvec col 0 pivot)
                         (subvec col (inc pivot)))
                   (update score scored-player (fnil + 0) (+ i removed-number))))

          :else
          (let [pivot (+ 2 prev-idx)]
            (if (= pivot cnt)
              (recur (inc i)
                     pivot
                     (into (into (subvec col 0 pivot) [i])
                           (subvec col pivot))
                     score)
              (let [pivot' (mod pivot cnt)]
                (recur (inc i)
                       pivot'
                       (into (into (subvec col 0 pivot') [i])
                             (subvec col pivot'))
                       score)))))))))

(def solve-memo (memoize solve))
(defn part-one
  [p n]
  (->> (solve-memo p (inc n))
       vals
       (apply max)))
#_(part-one 10 1618)
;; => 8317
#_(part-one 13 7999)
;; => 146373
#_(part-one 17 1105)
;; => 2720
#_(part-one 21 6111)
;; => 54718
#_(part-one 30 5807)
;; => 37305
#_(time (part-one 435 71184))
;; => 412959
;; 153676.88125 이전 풀이
;; 53360.58525 변경된 풀이
(println "step 2 is running")
(def res (part-one 435 7118400))
