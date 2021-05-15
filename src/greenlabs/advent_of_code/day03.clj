(ns greenlabs.advent-of-code.day03
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [greenlabs.advent-of-code.util :as util]))

(def sample-rawdata (str/split-lines "#1 @ 1,3: 4x4
#2 @ 3,1: 4x4
#3 @ 5,5: 2x2"))

(defn parse-input [input]
  (letfn [(parse [x]
            (->> x
                 (re-find #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")
                 rest
                 (mapv read-string)))]
    (map parse input)))

(defn parse2 [data]
  (let [data-v (mapv (fn [x]
                       (->> x
                            (re-find #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")
                            rest
                            (mapv read-string)))
                     data)
        data-s (map (fn [[id l t w h]]
                      {:id id
                       :square (set (for [a (range l (+ l w))
                                          b (range t (+ t h))]
                                      [a b]))})
                    data-v)]
    data-s))

;; S {:dup, :square}
;; S -> claim -> S'
;;
;; claims empty?
(defn part-one [claims]
  (loop [[claim & cs] claims
         dup #{}
         square #{}]
    (if-let [square-new (:square claim)]
      (let [square'(set/union square square-new)
            square-dup (set/intersection square square-new)
            dup' (set/union dup square-dup)]
        (recur cs dup' square'))
      dup)))

(count (part-one (parse2 sample-rawdata)))
;; => 4
(count (part-one (parse2 (util/slurp-resource-lines "day03.txt"))))
;; => 124850

;; 첫번째 변환 시도.
(defn part-one-reduce [claims]
  (let [claims' (mapcat :square claims)
        reduce-square (fn [acc i]
              (let [seen? (:seen? acc)]
                (if (seen? i)
                  (update-in acc [:dup] conj i)
                  (update-in acc [:seen?] conj i))))
        init {:seen? #{} :dup #{}}]
    (-> (reduce reduce-square
                init
                claims')
        :dup)
    ))

(part-one-reduce (parse2 sample-rawdata))

;; 두번째 변환 시도.
(defn part-one-reduce2 [claims]
  (let [claims' (mapcat :square claims)]
    (util/dup-all2 claims')
    ))

(part-one-reduce2 (parse2 sample-rawdata))

;; 1. square set을 만든다.
;; 2. 두번째 square를 덮는다.
;; 3. 반복한다.
;; 4. frequencies를 한다.
;; 5. count를 한다.

;; part two:
;; 하나도 겹치지 않은 square(직물)의 ID를 리턴하라
;; ID를 추가해서 loop를 도는 방법


;; [c1, c2, ...]
;; dup
(defn part-two [datas]
  (let [dup (part-one datas)]
    (:id (first (keep (fn [data]
                        (if (empty? (set/intersection dup (:square data)))
                          data
                          nil))
                      datas)))))

(defn part-two-reduce [datas]
  (let [dup (part-one-reduce datas)]
    (:id (first (keep (fn [data]
                        (if (empty? (set/intersection dup (:square data)))
                          data
                          nil))
                      datas)))))

(part-two (parse2 (util/slurp-resource-lines "day03.txt")))
(part-two-reduce (parse2 (util/slurp-resource-lines "day03.txt")))
