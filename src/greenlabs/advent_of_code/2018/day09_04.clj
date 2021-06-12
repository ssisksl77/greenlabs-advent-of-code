(ns greenlabs.advent-of-code.2018.day09-04)

(def Q (java.util.ArrayDeque.))
(def _map (java.util.HashMap.))

(defn rotate [i]
  ;(println ["rotate " i])
  (if (pos? i)
    (doseq [_ (range i)]
      (.addFirst Q (.removeLast Q)))
    (doseq [_ (range (- i))]
      (.addLast Q (.removeFirst Q)))))

#_(rotate -1)


(defn solve
  [p n]
  (doseq [i (range (inc n))]
    (cond
      (zero? i)
      (.addLast Q i)

      (= i 1)
      (.addLast Q i)

      (zero? (mod i 23))
      (let [player (mod i p)
            _      (rotate 7)
            rm-v (.removeLast Q)]
        (.put _map
              player
              (+ rm-v
                 (.getOrDefault _map player 0)
                 i))
        (rotate -1))

      :else
      (do
        (rotate -1)
        (.addLast Q i)))))


;(solve 9 25)
#_(solve 435 71184)
#_(apply max (vals _map))
;; 412959
(solve 435 7118400)
(apply max (vals _map))
