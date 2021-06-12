(ns greenlabs.advent-of-code.2018.day09-3
  (:import (java.util ArrayList HashMap)))


(def arr (ArrayList.))
(def _map (HashMap.))



(defn solve
  [p n]
  (doseq [i (range (inc n))]
    (cond
      (= i 0)
      (.add arr i)

;      (= i n)
;      nil

      (= 0 (mod i 23))
      (let [player (mod i p)
            rm-idx (dec (.size arr))
            _ (java.util.Collections/rotate arr 7)
            rm-v (.get arr rm-idx)]
        (.put _map
              player
              (+ rm-v
                 (.getOrDefault _map player 0)
                 i))
        (.remove arr rm-v)
        (java.util.Collections/rotate arr -1))

      :else
      (do
        (java.util.Collections/rotate arr -1)
        (.add arr i))
      )))

(solve 9 25)
#_(solve 10 1618)
#_(solve 30 5807)
#_(solve 435 71184) ;; 412959
;(solve 435 7118400)
(apply max (vals _map))

#_(def res (solve 10 1618))
