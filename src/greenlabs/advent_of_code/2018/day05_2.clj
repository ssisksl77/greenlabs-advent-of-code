(ns greenlabs.advent-of-code.2018.day05-2
  (:require [clojure.java.io :as io]))

(defn is-same-unit?
  [u1 u2]
  (= 32 (Math/abs (- (int u1) (int u2)))))

(def demo-input "dabAcCaCBAcCcaDA")
(def real-input (slurp (io/resource "day05.txt")))


;; 동일한 경우 res에서 pop을 합니다.
;; 동일하지 않은 경우 stack에 그 값을 추가합니다.
(defn part-one [input]
  (count (reduce (fn [res c]
                   (if (and (seq res)
                            (is-same-unit? c (peek res)))
                     (pop res)
                     (conj res c)))
                 '()
                 input)))

(time (part-one real-input))

(defn part-two
  [input]
  ;; #{\a \A} 형태의 range를 만듭니다.
  (let [alpha-range  (map (fn [c]  #{(char c) (char (+ c 32))})
                          (range (int \A) (inc (int \Z))))
        ;; 만든 range를 이용하여 문자열을 제거합니다.
        removed-strs (map (fn [x] (apply str (remove x input)))
                          alpha-range)]
    ;; 제거된 문자열을 part-one에 적용합니다. 
    (apply min (map part-one
                    removed-strs))))

(time (part-two real-input)) ;; 5412
