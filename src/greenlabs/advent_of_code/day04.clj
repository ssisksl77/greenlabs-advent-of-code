(ns greenlabs.advent-of-code.day04
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [greenlabs.advent-of-code.util :as util]
            [clojure.java.io :as io]))

; --- Day 4: Repose Record ---
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
(defn make-guards-shift-sheets [input]
  (letfn [(make-sheet  [guard-shift]
            (let [[guard & sleeps] (str/split-lines guard-shift)
                  id               (re-find #"\d+" guard) #_(subs guard 1 3)
                  sleeps'          (partition 2 (map #(Long/parseLong (subs % 15 17)) sleeps))]
              [(keyword id) sleeps']))
          (grouping-guards [sheet]
            (reduce (fn [acc [id sleeps]]
                      (update-in acc [id] into sleeps)) {} sheet))]
    (->> input
         (map make-sheet)
         grouping-guards)))

;; parsed data
(make-guards-shift-sheets demo-guards)

;; sleep-time
(defn organize-sheet [guard-shift-sheets]
  (let [sheet guard-shift-sheets]
    (map (fn [[id sleeps]]
           {:id     id
            :sheet sleeps
            :sleep-time (apply + (map (fn [[start end]]
                                        (let [n (- end start)]
                                          (if (pos? n)
                                            n
                                            (+ n 60))))
                                      sleeps))})
         sheet)))

(defn find-sleepyhead-guard [organized-sheets]
  (reduce (fn [acc e] (if (< (:sleep-time acc) (:sleep-time e))
                        e
                        acc))
          {:sleep-time 0}
          organized-sheets))

(defn dup-sleep-minute [guard]
  (let [sleepyhead-guard      guard
        sleep-range           (map (fn [[start end]] (range start end)) (:sheet sleepyhead-guard))
        sleep-min-frequencies (frequencies (reduce (fn [acc e]
                                                     (apply conj acc e))
                                                   []
                                                   sleep-range))]
    (first  (reduce (fn [acc [min frequencies]]
                      (if (< (second acc) frequencies)
                        [min frequencies]
                        acc))
                    [0 0]
                    sleep-min-frequencies))
    ))

(defn part-one [input]
  (let [sleepyhead-guard (find-sleepyhead-guard (organize-sheet input))
        best-minute (dup-sleep-minute sleepyhead-guard)]

    (* (Integer/parseInt (name (:id sleepyhead-guard))) best-minute)))

(part-one (make-guards-shift-sheets (rest (str/split demo-input #" Guard "))))
(part-one (make-guards-shift-sheets
           (rest (str/split (slurp (io/resource "day04.txt"))
                            #" Guard "))
           ))


(let [sorted-input (str/join "\n"
                             (sort-by 
                              #(Long/parseLong (apply str
                                                (rest (re-find #"\[(\d{4})-(\d\d)-(\d\d) (\d\d):(\d\d)\]"                                                
                                                               %))))
                              
                              #_#(subs % 1 17)
                              (util/slurp-resource-lines "day04.txt")))
      guards-sheet (make-guards-shift-sheets (rest (str/split sorted-input
                                                              #" Guard ")))]
  [(:id (find-sleepyhead-guard (organize-sheet guards-sheet ))) (reduce + (map (fn [[a b]]
                    (let [n (- b a)]
                      (if (pos? n)
                        n
                        (+ n 60))))
                  (:sheet (find-sleepyhead-guard (organize-sheet guards-sheet )))))]
  #_(dup-sleep-minute (find-sleepyhead-guard (organize-sheet guards-sheet)))
  (part-one guards-sheet)
  )
