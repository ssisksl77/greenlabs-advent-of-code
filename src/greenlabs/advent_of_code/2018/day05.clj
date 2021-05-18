(ns greenlabs.advent-of-code.2018.day05
  (:require [clojure.java.io :as io]))

" 엘프들은 수트사이즈를 줄이는데 애를 먹고 있다
폴리머의 화학구조를 볼 수 있는 듯

폴리머에는 유닛들이 있는데 트리거를 주면 두개의 이웃에게 영향을 준다.
인접한 두 이웃에게 영향을 주는데 
같은 유닛이면서 다른 극성은 사라짐.

단위 유형은 문자로 표시됩니다. 단위의 극성은 대문자로 표시됩니다.
예를 들어 r과 R은 동일한 유형이지만 극성이 반대 인 단위 인 반면 r과 s는 완전히 다른 유형이며 반응하지 않습니다."

(defn is-same-unit
  ([unit1 unit2]
   (and unit1
        unit2
        (= 32 (Math/abs
               (- (int unit1) (int unit2))))))
  ([a b c]
   (and (is-same-unit a b) (is-same-unit b c))))

(defn remove-diff-poly [input]
  (let [{:keys [_ value]} input]
    (loop [left []
           col  (vec value)]
      (if (seq col)
        (let [[a b] col]
          (cond
            (is-same-unit a b) {:done false
                                :value (apply str (into left (subvec col 2)))}
            :default (recur (into left (subvec col 0 1)) (subvec col 1))))
        {:done true
         :value (apply str value)}))))

(comment
  (remove-diff-poly {:done nil :value "dabAcCaCBAcCcaDA"})

  (= {:done false :value "dabAaCBAcCcaDA"}
     (remove-diff-poly {:done nil :value "dabAcCaCBAcCcaDA"})) ;

  (= {:done false :value "dabCBAcCcaDA"}
     (remove-diff-poly {:done nil :value "dabAaCBAcCcaDA"}))

  (= {:done false :value "dabCBAcaDA"}
     (remove-diff-poly {:done nil :value "dabCBAcCcaDA"})) ; dabCBAaDA

  (= {:done true :value "dabCBAcaDA"}
     (remove-diff-poly {:done false :value "dabCBAcaDA"})))

#_(defn part-one [input]
    (count (:value (last (vec (take-while  #(not (:done %))
                                           (iterate remove-diff-poly {:done nil :value input})))))))

(defn part-one [input]
  (->> {:done nil :value input}
       (iterate remove-diff-poly)
       (take-while  #(not (:done %)))
       vec
       last
       :value
       count))

#_(part-one "dabAcCaCBAcCcaDA")
(time (part-one (slurp (io/resource "day05.txt"))))


;; part two
;; One of the unit types is causing problems;
;; it's preventing the polymer from collapsing as much as it should.
;; Your goal is to figure out which unit type is causing the most problems, 
;; remove all instances of it (regardless of polarity), 
;; fully react the remaining polymer, and measure its length.

(defn remove-diff-and-collect-poly [input]
  (let [{:keys [cnt value]} input]
    (loop [left []
           cnt' cnt
           col  (vec value)]
      (if (seq col)
        (let [[a b] col]
          (cond
            (is-same-unit a b) {:done false
                                :cnt (update cnt' a (fnil inc 0))
                                :value (apply str (into left (subvec col 2)))}
            :default (recur (into left (subvec col 0 1)) cnt' (subvec col 1))))
        {:done true
         :cnt cnt
         :value (apply str value)}))))


#_(defn part-two [input]
  (->> {:done nil :cnt {} :value input}
       (iterate remove-diff-and-collect-poly)
       (take-while  #(not (:done %)))
       vec
       last
       #_:value
       #_count))

#_(part-two (slurp (io/resource "day05.txt")))



#_(def part-two-demo "dabAcCaCBAcCcaDA")
#_(apply str (remove #{\a \A} part-two-demo))

#_(part-one (apply str (remove #{\a \A} part-two-demo)))


#_(let [str-set (set (slurp (io/resource "day05.txt")))]
  (count str-set))

#_(range (int \a) (int \z))


(defn part-two [m]
  (let [res (into m {:cnt (part-one (:value m))})]
    (prn res)
    res))

(let [input (slurp (io/resource "day05.txt"))
      alpha-range (map (fn [c]  #{(char c) (char (+ c 32))})
                       (range (int \A) (inc (int \Z))))
      removed-strs (map (fn [x] (apply str (remove x input))) alpha-range)]
  (apply min (pmap part-one removed-strs))

  #_(for [str removed-strs]
      str)
  #_(part-one (first removed-strs))
  #_(map part-one removed-strs))




;; 다시풀기
