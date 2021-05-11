(ns greenlabs.advent-of-code.day03
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [greenlabs.advent-of-code.util :as util]
            [clojure.java.io :as io]))

(def sample-rawdata (str/split-lines "#1 @ 1,3: 4x4
#2 @ 3,1: 4x4
#3 @ 5,5: 2x2"))

;; [1 3 4 4] => left 1 right 1 + 5 top 3 bottom 3 + 7 => [1 3 6 10]
;; [3 1 4 4] => left 3 right 3 + 4 top 1 bottom 1 + 4 => [3 1 1  5]
;; [5 5 2 2] => left 5 right 5 + 2 top 2 bottom 5 + 2 => [5 2 7  7]
(defn parse [data]
  "ex) input: [\"#1 @ 5,5: 2x2\"]
output: ({:id 1, :square #{[6 6] [6 5] [5 6] [5 5]}})"
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

#_(parse ["#1 @ 5,5: 2x2"])

(defn part-one [datas]
  (loop [datas datas
         dup? #{}
         square #{}]
    (if-let [square-new (:square (first datas))]
      (let [square-union (set/union square square-new)
            square-dup (set/intersection square square-new)]
        (recur (rest datas) (set/union dup? square-dup) square-union))
      dup?)))

(count (part-one (parse sample-rawdata)))
;; => 4
(count (part-one (parse2 (util/slurp-resource-lines "day03.txt"))))
;; => 124850

;; 1. square set을 만든다.
;; 2. 두번째 square를 덮는다.
;; 3. 덮은 곳은 count를 올린다.
;; 4. filter로 count = 0 인 곳을 없앤다(?)
;; 5. count를 한다.

;; part two:
;; 하나도 겹치지 않은 square(직물)의 ID를 리턴하라
;; ID를 추가해서 loop를 도는 방법
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

(re-find #"(\d+) @ (\d+),(\d+): (\d+)x(\d+)" "#2 @ 3,1: 4x4")

(defn part-two [datas]
  (let [dup (part-one datas)]
    (:id (first (keep (fn [data]
                        (if (empty? (set/intersection dup (:square data)))
                          data
                          nil))
                      datas)))))

(comment
  (part-one (parse2 sample-rawdata))
  (part-two (parse2 sample-rawdata))

  (let [dup (part-one (parse2 (util/slurp-lines (io/resource "day03.txt"))))]
    (empty? (set/intersection dup (:square (first (parse2 (util/slurp-lines (io/resource "day03.txt")))))))))

(part-two (parse2 (util/slurp-resource-lines "day03.txt")))
