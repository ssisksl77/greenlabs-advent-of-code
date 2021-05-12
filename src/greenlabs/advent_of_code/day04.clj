(ns greenlabs.advent-of-code.day04
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [greenlabs.advent-of-code.util :as util]
            [clojure.java.io :as io]))

; --- Day 4: Repose Record ---
"
다른 벽장에 숨어들어감.
이번에는, 프로토타입 슈트 제조 랩 맞은편에 있음.
당신은 랩실 안으로 들어가서 슈트의 문제를 고쳐야 한다..
하지만 랩실 밖에 가드가 있어서, 안전한만큼 최대한 가까이 접근해야 한다.

옷장을 뒤지면서 도움이 될만한 것을 찾아보니,
숨어서 들어가고 싶은 사람이 혼자가 아님을 알았다.

벽을 덮으면서, 지난 몇 달 동안 누군가 이 초소를 몰래 감시하는 동안 매일 자정부터 한 시간을 보냈다.
They've been writing down the ID of the one guard on duty that night
가드의 ID가 그날 밤의 경비원 ID를 다 적어놓음.
엘프들은 한명의 가드면 야간 시프트를 충분히 할 수 있다고 결정한 것 같다.

as well as when they fall asleep or wake up while at their post (your puzzle input) .
야간시프트 뿐만 아니라, 맡은 자리에서 그들이 잘때나 일어날 때도...

예를들어 시간순으로 정리된 다음 레코드를 보자.

가드는 자는 시간이 00:25 이면 00:25 그 시간에는 깨어 있다고 할 수 있다.
특정시간에 잠들어 있을 가능성이 가장 높은 경비원을 알아낼 수 있다면,
밤에 그 밤에 그 경비원을 속여서 일을 시키고, 몰래 들어갈 수 있는 최상의 기회를 얻을 것.

최적의 가드/최적의 시간(분)을 선택하는 두 가지 전략이 있다.

전략 1.  몇 분 동안 잠든 경비원을 찾아라.
예로 가드 #10은 대부분의 시간(분)을 자는데 사용한다. total of 50 minutes( 20 + 20 + 5) 임.
가드 #99는 30분(10+10+10)
가드 #10은 00:24 에는 자고 있을 확률이 높음.

예제에서는 시간순으로 나열하지만, 항목(?)은 찾은 순서대로 표시됨.
 "

(def demo-input "[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up")

"part one : 가장 많이 주무신 가드의 ID와 주무신 시간(분단위)를 곱하라"

;; 1. 가드 단위로 partition을 나눌 수 있나?
(def demo-guards (rest (str/split demo-input #" Guard ")))

;; 1. guard-shift-sheet를 만든다.
(defn make-guards-shift-sheet [guard-shift]
  (let [[guard & sleeps]  (str/split-lines guard-shift)
        id (subs guard 1 3)
        sleeps' (partition 2 (map #(Long/parseLong (subs % 15 17)) sleeps))
        ]
    [id sleeps']
    #_{:guard-id (read-string id)
                      :sleeps sleeps'}))

(sort-by first (map make-guards-shift-sheet demo-guards))





(defn sleep-time [guards-shift-sheet]
  (let [[guard-id sleeps] guards-shift-sheet]
    [guard-id (reduce (fn [acc [s w]]
                        (+ acc (- w s)))
                   0
                   sleeps)]))

(defn max-sleep-guard [guard-sheet]
  (map
   #(reduce (fn [acc [_ n]]
              (update-in acc [:sleeps] + n))
            {:guard-id (first (first %))
             :sleeps 0}
            %)
   (vals guard-sheet)))

#_(def most-sleeps-guard
  (reduce (fn [acc e]
            (if (< (:sleeps acc) (:sleeps e))
              e
              acc))
          (max-sleep-guard guard-sleep)))

(defn find-member [col key]
  (loop [col col]
    (if-let [e (first col)]
      (if (= (first e) key)
        e
        (recur (next col))))))

#_(defn part-one [guards]
  (let [guards-shift-sheet (map make-guards-shift-sheet guards)
        guards-sleep-time (group-by first (map sleep-time guards-shift-sheet))
        guard-sleep-most (reduce (fn [acc e]
                                   (if (< (:sleeps acc) (:sleeps e))
                                     e
                                     acc))
                                 (max-sleep-guard guard-sleep))
        guard-sleep-range (duplicate-sleep-minute
                           (mapcat second (second (find-member (group-by first
                                                                         guards-shift-sheet)
                                                               (:guard-id guard-sleep-most)))))]

    (*  (read-string (:guard-id most-sleeps-guard)) (first (:dup (find-dup guard-sleep-range))))))

(part-one demo-guards)
(part-one (rest (str/split (slurp (io/resource "day04.txt")) #" Guard ")))


(let [part-one-input (rest (str/split (slurp (io/resource "day04.txt")) #" Guard "))]
  (map make-guards-shift-sheet part-one-input))


(comment
  (* (read-string (:guard-id most-sleeps-guard)) (first (:dup (find-dup sleep-range))))
  (def sleep-range (duplicate-sleep-minute
                    (mapcat second  (second (find-member
                                             (group-by first
                                                       (map make-guards-shift-sheet guards))
                                             "10")))
                    #_(second (find-member
                               (map make-guards-shift-sheet guards)
                               "10"))
                    )))
(def guard-sleep
  (->> demo-guards
       (map make-guards-shift-sheet)
       (map sleep-time)
       (group-by first)))
guard-sleep
(map make-guards-shift-sheet guards)

(def most-sleeps-guard
  (reduce (fn [acc e]
            (if (< (:sleeps acc) (:sleeps e))
              e
              acc))
          (max-sleep-guard guard-sleep)))

(guard-sleep (:guard-id most-sleeps-guard))



(def input (map make-guards-shift-sheet guards))

(sleep-time (first input))



(find-member
 (group-by first (map make-guards-shift-sheet guards))
 "10")


(second (find-member
                      (group-by first
                                (map make-guards-shift-sheet guards))
                      "10"))
(mapcat second  (second (find-member
                      (group-by first
                                (map make-guards-shift-sheet guards))
                      "10")))

(second (find-member
         (map make-guards-shift-sheet guards)
         "10"))

(defn duplicate-sleep-minute [sleeps]
  (map (fn [x] (range (first x) (second x))) sleeps))



(mapcat second  (second (find-member
                      (group-by first
                                (map make-guards-shift-sheet guards))
                      "10")))

(def sleep-range (duplicate-sleep-minute
                  (mapcat second  (second (find-member
                      (group-by first
                                (map make-guards-shift-sheet guards))
                      "10")))
                   #_(second (find-member
                            (map make-guards-shift-sheet guards)
                            "10"))
                   ))


(defn find-dup [cols]
  (let [sets (map set cols)]
    (reduce (fn [acc s]
              (let [dup (set/intersection (:seen? acc) s)]
                (prn dup)
                (if dup
                  {:seen? (set/union (:seen? acc) s)
                   :dup (set/union (:dup acc) dup)}
                  (update-in acc [:seen?] set/union s)
                  )
                ))
            {:seen? #{}
             :dup #{}}
            sets)))

sleep-range
(:guard-id most-sleeps-guard)
(* (read-string (:guard-id most-sleeps-guard)) (first (:dup (find-dup sleep-range))))



(subs "[1518-09-11 00:16] falls asleep" 1 17)

(sort-by #(subs % 1 17) (util/slurp-resource-lines "day04.txt"))
