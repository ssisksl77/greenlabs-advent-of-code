(ns greenlabs.advent-of-code.2018.day08
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;; add up all metadatas
(defn parse [input]
  (str/split input #" "))

(def demo-input (->> (parse "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2")
                     (map #(Integer/parseInt %))))

(def real-input (->>  (io/resource "2018/day08.txt")
                      slurp
                      parse
                      (map #(Integer/parseInt %))))
(defn dfs [root]
  (loop [node root]
    (when-let [[x & xs] (seq node)]
      (cond (coll? x) (recur (concat x xs))
            :else (do
                    (prn x)
                    (recur xs))))))

;; 1. child가 없으면 meta-data를 리턴할 수 있다.
;; 2. child가 여러개라면 child순회를 그만큼 돌아야 한다.
;; 3. child순회가 다 돌면 그제서야 meta-data를 알 수 있다.
;; - dfs로 어디에서 끝났는지 알아야 한다.
;; stack으로 meta-num을 넣어 놓는다?
;; 문제는 돌아와도 여기서 meta-data를 뺄 수가 없다는 점이다.

;; trampoline을 쓸 수는 없나?
(declare is-even?)

(defn is-odd? [n]
  (if (zero? n)
    false
    #(is-even? (dec n))))

(defn is-even? [n]
  (if (zero? n)
    true
    #(is-odd? (dec n))))

(trampoline (is-odd? 10000))


;; child-num = 0, n = 0 그대로 리턴
;; child-num > 0, n = 0 재귀하고 가져온 값에서 나의 메타데이터를 구한다.
;; child-num = 0, n > 0 재귀를 하긴 해야 하는데, n = 0이 될 때까지 재귀를 해야 한다.
;;   - n = 3이면, 3 - 2 - 1 - 0 으로 가야 한다.
;;   - 가면서 remainig 데이터도 바뀌어야 한다. 즉, 계산 후에 n - 1로 보내서 재귀를 돈다.
;;   - 0 이 되면 값을 알아서 리턴할 것이다.
;; child-num > 0, n > 0 내 생각에는 child-num부터 처리해야 한다. child-num이 있다면 바로
;;   - 재귀를 돌아야 한다. 그렇게 child-num이 0이 되었을 때, n을 줄이는 작업을 하기 위해서
;;   - 또 재귀를 돌 것이다.

(defn part-one' [[number-of-child number-of-meta & rest]]
  (if (zero? number-of-child)
    {:metadata (take number-of-meta rest)
     :restdata (drop number-of-meta rest)}
    (let [{m :metadata
           r :restdata}  (reduce (fn [{metadata :metadata restdata :restdata} _]
                                   (let [{md :metadata rd :restdata} (part-one' restdata)]
                                     {:metadata (conj metadata md)
                                      :restdata rd}))
                                 {:metadata nil
                                  :restdata rest}
                                 (range number-of-child))]
      {:metadata (conj m (take number-of-meta r))
       :restdata (drop number-of-meta r)})))

(apply + (flatten (:metadata (part-one' demo-input))))
(apply + (flatten (:metadata (part-one' real-input))))
;; 44338 solve


(part-one' demo-input)


(defn part-two [[number-of-child number-of-meta & rest]]
  (if (zero? number-of-child)
    {:v (apply + (take number-of-meta rest))
     :metadata (take number-of-meta rest)
     :restdata (drop number-of-meta rest)}
    (let [children  (reductions (fn [{metadata :metadata restdata :restdata} _]
                                  (let [{md :metadata rd :restdata
                                         v :v } (part-two restdata)]
                                    {:metadata md
                                     :v v
                                     :restdata rd}))
                                {:metadata nil
                                 :v 0
                                 :restdata rest}
                                (range number-of-child))
          {m :metadata
           v :v
           r :restdata} (last children)
          mds (take number-of-meta r)
          values (apply + (map (fn [idx]
                            (if-let [v (:v (nth children idx 0))]
                              v
                              0))
                          mds))]
      {:v values
       :metadata (take number-of-meta r) ;(conj m (take number-of-meta r))
       :restdata (drop number-of-meta r)})))

(part-two demo-input)
(part-two real-input)
;; 37560
