(ns greenlabs.advent-of-code.day02
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [greenlabs.advent-of-code.util :as util]))

(def demo-rawdata (str/split-lines "abcdef contains no letters that appear exactly two or three times.
bababc contains two a and three b, so it counts for both.
abbcde contains two b, but no letter appears exactly three times.
abcccd contains three c, but no letter appears exactly two times.
aabcdd contains two a and two d, but it only counts once.
abcdee contains two e.
ababab contains three a and three b, but it only counts once."))

(defn parse [input]
  (flatten (map (fn [input']
                  (vec (set (filter #(or (= % 2) (= % 3))
                                    (vals (frequencies input'))))))
                input)))

;; test
(comment
  (def demo-input (map (partial re-find #"\w+\s") demo-rawdata))
  (set (filter #(or (= % 2) (= % 3))
               (vals (frequencies "bababc"))))
  (parse demo-input))

;; 2번 겹치는 문자, 3번 겹치는 문자 횟수를 count하라
;; 같은 문자에서 2번 겹치는 문자가 여러번 나와도 count는 하나이다.
(defn part-one [input]
  (let [init-hash {2 0 3 0}
        {threes 3 twos 2} (reduce (fn [acc e]
                                      (update-in acc [e] inc))
                                    init-hash
                                    input)]
    (* threes twos)))

(part-one (parse (util/slurp-resource-lines "day02.txt")))

;; part two
;; 동일한 위치에 단 하나만 다른 문자를 찾아서 리턴하라. (다른 문자는 없애고)
(defn remove-index [s index]
  (let [left (subs s 0 index)
        right (subs s (inc index))]
    (str left right)))



;; 1. 문자열을 한번씩 제거를 해본다.
;; 2. 겹치는 문자를 확인한다.
(defn part-two [strs]
  (let [len (count (first strs))]
    (first (flatten (for [i (range len)]
                      (->> (map #(remove-index % i)
                                strs)
                           frequencies
                           (filter #(= (second %) 2))
                           ))))))


(part-two (util/slurp-resource-lines "day02.txt"))


(comment
  (part-two (str/split-lines (slurp (io/resource "day02.txt"))))

  (first (flatten (for [i (range 5)]
                    (->> (map #(remove-index % i)
                              (str/split-lines b))
                         frequencies
                         (filter #(= (second %) 2))
                         ))))

  (str/split-lines (slurp (io/resource "day02.txt"))))
c
