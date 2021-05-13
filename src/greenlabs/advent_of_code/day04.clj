(ns greenlabs.advent-of-code.day04
  (:require [clojure.string :as str]
            [greenlabs.advent-of-code.util :as util]))

; --- Day 4: Repose Record ---
"part one : 가장 많이 주무신 가드의 ID와 주무신 시간(분단위)를 곱하라"

;; 1. 인풋 자르기
(defn group-by-guard 
  "가드단위로 자른 근무일지를 리턴합니다."
  [input]
  (letfn [(get-date [guard-shift-string]
                    (->> (re-find #"\[(\d{4})-(\d\d)-(\d\d) (\d\d):(\d\d)\]" guard-shift-string)
                         rest
                         (apply str)))
          (sort-input [input]
                      (->> input 
                           (sort-by #(Long/parseLong (get-date %)))
                           (str/join "\n")))]
    
    (rest (str/split (sort-input input)
                     #" Guard "))
    ))

;; 2. 잠을 잔 범위 구하기
(defn make-guards-shift-sheets 
  " 가드별로 그룹핑한 숙면range 시트를 리턴합니다."
  [input]
  (letfn [(make-sheet  [guard-shift]
            (let [[guard & sleeps] (str/split-lines guard-shift)
                  id               (re-find #"\d+" guard)
                  sleeps'          (partition 2 (map #(Long/parseLong (subs % 15 17)) sleeps))]
              [(keyword id) sleeps']))
          (grouping-guards [sheet]
            (reduce (fn [acc [id sleeps]]
                      (update-in acc [id] into sleeps)) {} sheet))]
    (->> input
         (map make-sheet)
         grouping-guards)))

;; 3. 가드가 얼마나 잤는지 계산한다.
(defn organize-sheet 
  "가드 시트지에서 가드가 얼마나 잤는지(:sleep-time)을 계산하여 리턴합니다."
  [guard-shift-sheets]
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

;; 4. 가장 많이 잔 가드를 찾는다.
(defn find-sleepyhead-guard 
  "가장 많이 잔 가드를 리턴합니다"
  [organized-sheets]
  (reduce (fn [acc e] (if (< (:sleep-time acc) (:sleep-time e))
                        e
                        acc))
          {:sleep-time 0}
          organized-sheets))

;; 5. 가장 자는시간이 겹친 minute을 계산한다.
(defn dup-sleep-minute 
  "가장 자는 시간이 겹친 minute을 리턴합니다"
  [guard]
  (let [sleepyhead-guard      guard
        sleep-range           (map (fn [[start end]] (range start end)) (:sheet sleepyhead-guard))
        sleep-min-frequencies (frequencies (reduce (fn [acc e]
                                                     (apply conj acc e))
                                                   []
                                                   sleep-range))
        min-max-fn (fn [acc [min frequencies]]
                     (if (< (:fq acc) frequencies)
                       {:min min :fq frequencies}
                       acc))]
    (assoc guard :most-sleep-min
           (reduce min-max-fn
                   {:min 0
                    :fq 0}
                   sleep-min-frequencies))))
                   
(defn part-one 
  "가장 많이 잔 가드의 아이디와 가장 많이 잤던 분(minute)의 곱을 리턴합니다"
  [guards-sheet]
  (let [sleepyhead-guard (find-sleepyhead-guard (organize-sheet guards-sheet))
        best-minute (:min (:most-sleep-min (dup-sleep-minute sleepyhead-guard)))]

    (* (Integer/parseInt (name (:id sleepyhead-guard))) best-minute)))

#_(part-one (make-guards-shift-sheets (rest (str/split demo-input #" Guard "))))
#_(part-one (make-guards-shift-sheets
             (rest (str/split (slurp (io/resource "day04.txt"))
                              #" Guard "))))


(defn part-two
  "잠을 잤던 분(minute)이 겹친 횟수가 가장 많았던 가드의 id와 분(minute)의 곱을 리턴합니다"
  [guards-sheet]
  (let [dup-sleep-minutes (map dup-sleep-minute (organize-sheet guards-sheet))
        guard (reduce (fn [acc e]
                        (if (< (:fq (:most-sleep-min acc)) (:fq (:most-sleep-min e)))
                          e
                          acc))
                      dup-sleep-minutes)]

    (* (Integer/parseInt (name (:id guard))) (:min (:most-sleep-min guard)))))


(let [grouped-input (group-by-guard (util/slurp-resource-lines "day04.txt"))
      guards-sheet (make-guards-shift-sheets grouped-input)]
  (part-one guards-sheet)
  (part-two guards-sheet))
