(ns greenlabs.advent-of-code.day06
  (:require [clojure.string :as str]
            [greenlabs.advent-of-code.util :as util]
            [clojure.java.io :as io]))

;; start 10:30
;; end 11:00
;; start 12:45
(def demo-input (->> (str/split "1, 1
1, 6
8, 3
3, 4
5, 5
8, 9"
                                #"\n|\, ")
                     (map #(Integer/parseInt %))
                     (partition 2)))


(defn to-map [input]
  (map-indexed
   (fn [i [x y]] {:id i :x x :y y})
   input))

(to-map demo-input)

(defn manhattan-distance [[p1-x p1-y] [p2-x p2-y]]
  (+ (Math/abs (- p1-x p2-x)) (Math/abs (- p1-y p2-y))))


(manhattan-distance [1 1] [3 3])

(defn scattering [x y]
  (let [directions [[-1 -1] [0 -1] [1 -1]
                    [1 0] [1 1]
                    [0 1] [-1 1]
                    [-1 0]]]
    (map (fn [[dx dy]] [(+ x dx) (+ y dy)])
         directions)))

(scattering 1 1)


;; 왼쪽으로 퍼지기 - 갖고있는 가장 왼쪽 것들을 다 (-1, 0)
;; 오른쪽으로 - 갖고 있는 오른쪽 것들을 다 (1, 0)
;; 위 - 모두 (0,1)
;; 아래 - 모두 (0,-1)
;; 되려나...
;; . 이 있으면 그 이상 퍼질 수 없음. x라고 표시를 하던가 뭔가 해야 할듯

(defn scattering' [star]
  (partition-by first (sort star)))

(scattering' (scattering 1 1))


;; 무한행렬문제가 아님.
;; 한 칸이 있고, 그 칸에서 뻗어나갔으면 무한이다.
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
demo-input

;; 아니다 각 포인트마다 누가 제일 가까운지 확인하고
;; 테두리에 무낮가 있는 녀석은 infinite이라고 하고
;; 아니면 map으로 카운트한다.
;; 제일숫자가 큰 녀석을 리턴.
(def demo-input (->> (str/split "1, 1
1, 6
8, 3
3, 4
5, 5
8, 9"
                                #"\n|\, ")
                     (map #(Integer/parseInt %))
                     (partition 2)))


(defn manhattan-distance [[p1-x p1-y] [p2-x p2-y]]
  (+ (Math/abs (- p1-x p2-x)) (Math/abs (- p1-y p2-y))))

(defn make-square [input]
  (let [max-point (reduce (fn [acc e]
                            [(inc (max (first acc) (first e)))
                             (inc (max (second acc) (second e)))])
                          [0 0]
                          input)]
    (for [x (range (first max-point))
          y (range (second max-point))]
      [x y])))

(let [max-point (reduce (fn [acc e]
                          [(max (first acc) (first e))
                           (max (second acc) (second e))])
                        [0 0]
                        demo-input)]
  max-point)

demo-input
(make-square demo-input)
demo-input

(defn make-manhattan-distances [input]
  (let [square (make-square input)]
    (flatten (map-indexed (fn [idx-x A]
                            (map (fn [s] {:id idx-x
                                          :p  s
                                          :dt (manhattan-distance (vec A) s)})
                                 square))
                          input))))
#_(def manhattan-distances' (let [square (make-square demo-input)]
                              (flatten (map-indexed (fn [idx-x A]
                                                      (map (fn [s] {:id idx-x
                                                                    :p  s
                                                                    :dt (manhattan-distance (vec A) s)})
                                                           square))
                                                    demo-input))))


(defn calculate-manhattan-distances [manhattan-distances]
  (reduce (fn [acc e]
            (if-let [dt (get-in acc [(:p e) :dt])]
              (cond
                (< (:dt e) dt) (assoc acc (:p e) {:dt (:dt e) :id (:id e)})
                (= (:dt e) dt) (assoc acc (:p e) {:dt (:dt e) :id nil})
                :default acc)
              (assoc acc (:p e) {:id (:id e) :dt (:dt e)})))
          {}
          manhattan-distances))


#_(def a (reduce (fn [acc e]
                   (if-let [dt (get-in acc [(:p e) :dt])]
                     (cond
                       (< (:dt e) dt) (assoc acc (:p e) {:dt (:dt e) :id (:id e)})
                       (= (:dt e) dt) (assoc acc (:p e) {:dt (:dt e) :id nil})
                       :default acc)
                     (assoc acc (:p e) {:id (:id e) :dt (:dt e)})))
                 {}
                 manhattan-distances'))

;; 0,0 ... 0, max-y
;; 0,0 ... max-x 0
;; max-x,0 ... max-x, max-y
;; 0, max-y ... max-x, max-y
(defn infinite-pts [max-x max-y]
  (let [x-range (range max-x)
        y-range (range max-y)
        max-x-idx  (dec max-x)
        max-y-idx  (dec max-y)]
    (->> (concat (mapcat (juxt (fn [x] [x, 0]) (fn [x] [x, max-y-idx]))
                         x-range)
                 (mapcat (juxt (fn [y] [0, y]) (fn [y] [max-x-idx, y]))
                         y-range))
         set)))



(defn part-one [input cmd]
  (let [max-point (reduce (fn [acc e]
                            [(inc (max (first acc) (first e)))
                             (inc (max (second acc) (second e)))])
                          [0 0]
                          input)
        infinite-pts-set (infinite-pts (first max-point) (second max-point))
      ;; infinite인 녀석들을 셋으로 가져옴.
        filtered (set (keep (fn [[k v]] (when (infinite-pts-set k)
                                          (:id v)))
                            cmd))
        b (filter (fn [[_ v]] (let [id (:id v)]
                                (when-not (or (nil? id) (filtered id) (= -1 (:dt v)))

                                  v))) cmd)]
    (->> (group-by (fn [[_ v]] (:id v))
                   b)
         (map (fn [[_ v]] (count v)))
         (apply max))))


(defn parse [input]
  (let [input' (str/split input #"\n|\, ")]
    (->> input'
         (map #(Integer/parseInt %))
         (partition 2))))

(defn find-safe-pts [input cmd]
  (let [max-point (reduce (fn [acc e]
                            [(inc (max (first acc) (first e)))
                             (inc (max (second acc) (second e)))])
                          [0 0]
                          input)
        infinite-pts-set (infinite-pts (first max-point) (second max-point))
      ;; infinite인 녀석들을 셋으로 가져옴.
        filtered (set (keep (fn [[k v]] (when (infinite-pts-set k)
                                          (:id v)))
                            cmd))
        b (filter (fn [[_ v]] (let [id (:id v)]
                                (when-not (or (nil? id) (filtered id) (= -1 (:dt v)))

                                  v))) cmd)]
    b))


;; part one
#_(let [p (parse "1, 1
1, 6
8, 3
3, 4
5, 5
8, 9")
        mmd (make-manhattan-distances p)
        cmd (calculate-manhattan-distances mmd)]
    (part-one p cmd))

(defn neighbors [[x y]]
  (let [directions [[-1 0] [1 0] [0 -1] [0 1]]]
    (map (fn [[x' y']] [(+ x x') (+ y y')])
         directions)))

(defn part-two [safe-pts]
  (let [safe-pts-set (set safe-pts)]
    (prn [(count safe-pts) safe-pts-set])
    (loop [safe-pt safe-pts
           res #{}]
      #_(prn safe-pt)
      (if-let [valid-pt (safe-pts-set (first safe-pt))]
        (do
          #_(prn valid-pt)
          (recur (concat (rest safe-pt) (neighbors valid-pt))
                 (conj res valid-pt)))
        res))))

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


#_(def aa (let [safe-cnt 32
              p (parse "1, 1
1, 6
8, 3
3, 4
5, 5
8, 9")
              mmd (make-manhattan-distances p)
              cmd (calculate-manhattan-distances mmd)]
          #_(spit "demo.txt" (str/split-lines (apply str mmd)))
          (reduce (fn [acc e]
                    (let [k (:p e)
                          dt (get e :dt 0)]
                      #_(prn [acc k dt])
                      (if (acc k)
                        (update acc k + dt)
                        (assoc acc k dt))))
                  {}
                  mmd)))

(count (filter (fn [[k v]] (< v 32))
               (part-two (parse "1, 1
1, 6
8, 3
3, 4
5, 5
8, 9"))))

(part-two
 (parse (slurp (io/resource "day06.txt"))))

(count (filter (fn [[_ v]] (< v 10000))
               (part-two 
                (parse (slurp (io/resource "day06.txt"))))))

(def safe-pts (let [safe-cnt 1000
                    parsed-input
                    (parse (slurp (io/resource "day06.txt")))
                    #_(parse "1, 1
1, 6
8, 3
3, 4
5, 5
8, 9")
                    mmd (make-manhattan-distances parsed-input)
                    cmd (calculate-manhattan-distances mmd)
                    safe-pts (find-safe-pts parsed-input cmd)]
                (->> (for [safe-pt (map first safe-pts)]
                       (->> (apply + (map #(manhattan-distance safe-pt %)
                                          parsed-input))
                            (vector safe-pt)))
                     (filter (fn [[_ dt]]
                               (< dt safe-cnt)))
                     (map first)
                     part-two)))
safe-pts

(parse (slurp (io/resource "day06.txt")))
;; loop 
;; 1. is-safe-pts?에 걸리는 녀석이 있다면?
;; 1-1. 그 녀석의 neighbor를 추가해서 순회를 recur로 부른다.
;; 2. is-safe-pts?에 걸리는 녀석이 없다면?
;; 2-1. 현재 누적된 셋을 리턴한다.

(map (fn [safe-pt]
       (loop [pt safe-pt
              res #{}]
         pt))

     safe-pts)


(defn find-neighbors [safe-pt-dts]
  (let [safe-pts (map first safe-pt-dts)
        safe-pts-set (set safe-pts)
        neighbors-pt [[-1 0] [1 0] [0 -1] [0 1]]]
    (loop [safe-pt (safe-pt)]
      (map (fn [[x y]] [(+ (first safe-pt) x)
                        (+ (second safe-pt) y)])
           neighbors-pt))))

#_(for [safe-pt  safe-pts]
    (->> (map (fn [pi]
                (conj safe-pt :star-dts  (manhattan-distance (first safe-pt) pi)))
              parsed-input)))
