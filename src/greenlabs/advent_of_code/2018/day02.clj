(ns greenlabs.advent-of-code.2018.day02
  (:require [clojure.set :as set]
            [greenlabs.advent-of-code.util :as util]))

;; 2번 겹치는 문자, 3번 겹치는 문자 횟수를 count하라
;; 같은 문자에서 2번 겹치는 문자가 여러번 나와도 count는 하나이다.
(defn part-one [input]
  (letfn [(count-two-threes
            [input]
            (flatten (map (fn [input']
                            (vec (set (filter #(or (= % 2) (= % 3))
                                              (vals (frequencies input'))))))
                          input)))]

    (let [two-threes (count-two-threes input)
          init-hash {2 0 3 0}
          {threes 3 twos 2} (reduce (fn [acc e]
                                      (update-in acc [e] inc))
                                    init-hash
                                    two-threes)]
      (* threes twos))))

(part-one (util/slurp-resource-lines "day02.txt"))

;; merge-with 사용.
;; merge-with + 로 바꾸는 장애물
;;   장애1. frequencies로 만들었을 때, value에 2가 두 번생성될 때 한번으로 카운트 해야 한다.
;;         frequencies 값을 그대로 (merge + )에 적용할 수 없다.
;;         한번 필터링을 해야한다.
(defn part-one2 [input]
  (let [{twos 2 threes 3} (->> input
                               (map frequencies)
                               (map #(set/map-invert %))
                               (map #(select-keys % [2 3]))
                               (apply merge-with (fn [x _] (inc x)) {2 0 3 0}))]
    (* twos threes)))

(part-one2 (util/slurp-resource-lines "day02.txt"))

;; part two
;; 동일한 위치에 단 하나만 다른 문자를 찾아서 리턴하라. (다른 문자는 없애고)
(defn remove-index [s index]
  (let [left (subs s 0 index)
        right (subs s (inc index))]
    (str left right)))

(defn for-i [f len]
  (for [i (range len)]
    (f i)))

;; 1. 문자열을 한번씩 제거를 해본다.
;; 2. 겹치는 문자를 확인한다.
(defn part-two2 [strs]
  (let [len  (count (first strs))
        strs (for-i (fn [i]
                      (map #(remove-index % i) strs))
                    len)]
    (keep #(util/dup-one %) strs)))


;; map(f) => 
;; map(g) => g: 'a => boolean
;; filter(x => x)
;; count 

;; filter(g') => g': 'a => 'a

;; ["" "" "" ...] -> 겹치는 문자가 나오면 리턴
;;
;;  day01.clj도 비슷한게 있었는데? 공통 함수로 리팩토링 해보기
(part-two2 (util/slurp-resource-lines "day02.txt"))

;; map-indexed로 변경.
(defn part-two3 [strs]
  (let [strs (map-indexed (fn [idx _]
                            (map #(remove-index % idx) strs))
                          (first strs))]
    (keep #(util/dup-one %) strs)))

(part-two3 (util/slurp-resource-lines "day02.txt"))

(= (part-two2 (util/slurp-resource-lines "day02.txt"))
   (part-two3 (util/slurp-resource-lines "day02.txt")))

