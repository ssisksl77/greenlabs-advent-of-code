(ns greenlabs.advent-of-code.day06
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;; 무한행렬문제가 아님.
;; 하나의 사각칸이 있고, 그 칸에서 뻗어나갔으면 무한이다.
;; 그럼 일단 무한이 아닌 녀석만 찾으면 된다.
;; 어떻게 찾을까?
;; 두 점이 있으면 그 점 사이에는 finite이다.
;; 두 점 사이는 한면 혹은 두개의 면을 걸칠 수 있다.
;; 한 점과 두 점 사이인지 확인하는 방법은? 거리가 (1,1) 같이 대각선으로 만나는 것이 아니면 
;; 대체적으로 한면을 주로 겹친다.
;; 겹치는 면이 문제가 아니라 inifinite과 finite을 구하는 것이 문제이다.
;; 내가 한면이 끝에 닿으면 infinite이다.
;; 한면에 하나도 닿지 않으면 finite이다.
;; 이렇게 하려면 결국 별을 키워야겠다.

;; 아니다 각 포인트마다 누가 제일 가까운지 확인하고
;; 테두리에 문자가 있는 녀석은 infinite이라고 하고
;; 아니면 map으로 카운트한다.
;; 제일숫자가 큰 녀석을 리턴.
(defn manhattan-distance 
  "두 xy-좌표의 맨하탄거리를 리턴합니다."
  [[p1-x p1-y] [p2-x p2-y]]
  (+ (Math/abs (- p1-x p2-x)) (Math/abs (- p1-y p2-y))))

(defn make-square 
  "input좌표들 중 최대/최소 x,y를 이용하여 사각형 범위를 리턴합니다."
  [input]
  (let [max-point (reduce (fn [acc e]
                            [(inc (max (first acc) (first e)))
                             (inc (max (second acc) (second e)))])
                          [0 0]
                          input)]
    (for [x (range (first max-point))
          y (range (second max-point))]
      [x y])))

(defn make-manhattan-distances 
  "각 별을 기준으로 맨하탄거리를 모두 계산하여 리턴합니다."
  [input]
  (let [square (make-square input)]
                            (flatten (map-indexed (fn [idx-x A]
                            (map (fn [s] {:id idx-x
                                          :p  s
                                          :dt (manhattan-distance (vec A) s)})
                                 square))
                          input))))

(defn calculate-manhattan-distances 
  "각 별을 기준으로 계산된 맨하탄거리들을 비교하여 :dt 숫자가 작은 별로로 대체됩니다.
   동일한 경우 별의 이름은 nil로 대체합니다.
   
  :id  - ?
  :p   - ?
  :dt  - ?

   "
  [manhattan-distances]
  (reduce (fn [acc e]
            (if-let [dt (get-in acc [(:p e) :dt])]
              (cond
                (< (:dt e) dt) (assoc acc (:p e) {:dt (:dt e) :id (:id e)})
                (= (:dt e) dt) (assoc acc (:p e) {:dt (:dt e) :id nil})
                :default acc)
              (assoc acc (:p e) {:id (:id e) :dt (:dt e)})))
          (sorted-map)
          manhattan-distances))

;; 0,0 ... 0, max-y
;; 0,0 ... max-x 0
;; max-x,0 ... max-x, max-y
;; 0, max-y ... max-x, max-y
(defn infinite-pts 
  "무한으로 뻗어나가는 별을 구하기 위해 (0,0)-(max-x, max-y) 사각형의 테두리 좌표들을 리턴합니다."
  [max-x max-y]
  (let [x-range (range max-x)
        y-range (range max-y)
        max-x-idx  (dec max-x)
        max-y-idx  (dec max-y)]
    (->> (concat (mapcat (juxt (fn [x] [x, 0]) (fn [x] [x, max-y-idx]))
                         x-range)
                 (mapcat (juxt (fn [y] [0, y]) (fn [y] [max-x-idx, y]))
                         y-range))
         set)))

(defn parse
  "문자열을 xy쌍 배열리스트로 변형하려 리턴합니다."
  [input]
  (let [input' (str/split input #"\n|\, ")]
    (->> input'
         (map #(Integer/parseInt %))
         (partition 2))))

(defn find-safe-pts 
  "무한으로 뻗어나가는 별을 제거합니다.
   별 단위로 그룹을 만든 이후 가장 큰 숫자를 리턴합니다."
  [input cmd]
  (let [max-point (reduce (fn [acc e]
                            [(inc (max (first acc) (first e)))
                             (inc (max (second acc) (second e)))])
                          [0 0]
                          input)
        infinite-pts-set (infinite-pts (first max-point) (second max-point))
      ;; infinite인 녀석들을 셋으로 가져옴.
        infinite-stars-set (set (keep (fn [[k v]] (when (infinite-pts-set k)
                                          (:id v)))
                            cmd))
        safe-stars (filter (fn [[_ v]] (let [id (:id v)]
                                         (when-not (or (nil? id) (infinite-stars-set id) (= -1 (:dt v)))
                                           v))) cmd)]
    safe-stars))

(def demo-input (parse "1, 1
1, 6
8, 3
3, 4
5, 5
8, 9"))
(def real-input (parse (slurp (io/resource "day06.txt"))))

;; part one
(defn part-one [input]
  (let [cmd (->> input
                 make-manhattan-distances
                 calculate-manhattan-distances)
        safe-pts (find-safe-pts input cmd)]
    (->> (group-by (fn [[_ v]] (:id v))
                   safe-pts)
         (map (fn [[_ v]] (count v)))
         (apply max))))

(part-one demo-input)
(part-one real-input)  ;; 2342
;; part two
(defn part-two [input]
  (let [p input
        mmd (make-manhattan-distances p)]
    (reduce (fn [acc e]
              (let [k (:p e)
                    dt (get e :dt 0)]
                (if (acc k)
                  (update acc k + dt)
                  (assoc acc k dt))))
            {}
            mmd)))

(count (filter (fn [[_ v]] (< v 32))
               (part-two demo-input)))

;; part two - manhattan distance를 전부 더한 다음에 10000 밑인 점들을 찾는다.
(count (filter (fn [[_ v]] (< v 10000))
               (part-two 
                (parse (slurp (io/resource "day06.txt"))))))  ;; 44302                                              