(ns aoc-2018.day07
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.java.io :as io]))

;; --- Day 7: The Sum of Its Parts ---



;; 위상정렬문제인듯.
;; {B #{A}, E #{B D F}, D #{A} F #{C}}
(defn parse [input]
  (let [lines (str/split-lines input)]
    (->> lines
         (map #(->> %
                    (re-find #".+([A-Z]).+([A-Z])")
                    rest))
         #_(map (fn [[to from]]
                  {:from from :to to}))
         (reduce (fn [acc [to from]]
                   (if (acc from)
                     (update acc from conj to)
                     (conj acc {from #{to}})))
                 {}))))
(def demo-input (parse "Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.
"))
(def real-input (parse (slurp (io/resource "day07.txt"))))



;; 1. graph를 만든다. (parse)
;; 2. 의존성이 없는 요소를 찾는다. (to를 재귀적으로 순회 후 from이 없는 요소를 찾는다)
;; 3. 요소가 없는 것부터 탐색을 시작한다.
;; http://dnaeon.github.io/graphs-and-clojure/

;; to가 먼저 실행되는 것.
(defn no-deps-items
  [graph]
  (let [tos (reduce set/union (vals graph))
        froms (set (keys graph))]
    (filter #(not (froms %)) tos)))

(no-deps-items (parse demo-input))


;; value에 items-to-remove가 있다면 지워야 한다.
;; {B #{A}, E #{B D F}, D #{A} F #{C}}
;; **
;; 그래서 하나씩만 지우는 녀석을 찾는다.
(defn remove-item-from-deps [graph item-to-remove]
  (let [remove-set (set (list item-to-remove))]
    (->> graph
         (map (fn [[from to]]
                (let [remaining-tos (set/difference to remove-set)]
                  [from remaining-tos])))
         (reduce (fn [acc [from to]]
                   (if (seq to)
                     (update acc :remaining-vertexes conj {from to})
                     (update acc :no-deps conj from)))
                 {:no-deps []
                  :remaining-vertexes {}}))))


(map (fn [[from to]]
       (set/difference to))
     {"A" #{"A" "C" "E"} "B" #{"B" "E" "C"}})

;; items-to-remove가 g에서 빠지려면 어떻게 해야 하나?
;; 문제점이 있다. 서로 의존성이 없는 경우는 알파벳우선으로 나와야 한다...
(defn top-sort [graph]
  (loop [g graph
         no-deps-items_  (no-deps-items g) #_(into '() (no-deps-items g)) ;; lazy-seq
         res '()]
    #_(prn {:no-deps no-deps-items_
            :g g
            :r res})
    (if (seq no-deps-items_)
      (let [sorted-deps (sort no-deps-items_)
            item-to-remove (first sorted-deps)
            removed (remove-item-from-deps g item-to-remove)]
        #_(prn {:itr item-to-remove, :remain removed})
        (recur (:remaining-vertexes removed)
               (into (rest sorted-deps) (:no-deps removed))
               (concat res item-to-remove)))
      res)))
;; part one
(top-sort demo-input)
(apply str (top-sort demo-input))
(apply str (top-sort real-input))

;;시도4 SCLPAMQVUWNHODRTGYKBJEFXZI  성공
;;시도3 SCLVWNPAMHODRQTGUYKBJEFXZI
;;시도2 SCPYMALWNVHODRQUTGKBJEFXZI
;;시도1 SCLVWNPQMHOTUYAGKBDRJEFXZI

;; part two

;; second와 until을 비교하여 busy한지 아닌지를 확인한다.
(defn remove-item-from-deps2 [graph item-to-remove]
  (let [item-to-remove-list (if (list? item-to-remove)
                              (map (fn [x] (.charAt x 0))
                                   item-to-remove)
                              item-to-remove)
        remove-set (set item-to-remove-list)]
    (->> graph
         (map (fn [[from to]]
                (let [remaining-tos (set/difference to remove-set)]
                  [from remaining-tos])))
         (reduce (fn [acc [from to]]
                   (if (seq to)
                     (update acc :remaining-vertexes conj {from to})
                     (update acc :no-deps conj from)))
                 {:no-deps '()
                  :remaining-vertexes {}}))))

(defn make-workers [n]
  (reduce (fn [acc e]
            (assoc acc e {:until -1 :job nil}))
          {}
          (range n)))

(defn time-to-finish [word]
  (let [c (if (char? word) word (.charAt word 0))]
    (+ 60 (- (int c) 64))))
(time-to-finish "Z")
(time-to-finish "A")
;; 1. sec와 동일한 until값이 있는 녀석은 {:done}으로 리턴할 것.
(defn update-workers [workers sec]
  (let [is-finished? (fn [e] (when (= sec (:until e))
                               e))
        finished-jobs (keep (fn [[k v]] (is-finished? v))
                            workers)
        updated-workers (reduce (fn [acc [k v]]
                                  (if (is-finished? v)
                                    (assoc acc k {:until -1 :job nil})
                                    (assoc acc k v)))
                                {}
                                workers)]
    {:workers updated-workers
     :finished-jobs finished-jobs}))
(update-workers {0 {:until 3, :job "C"}, 1 {:until -1, :job nil}, 2 {:until -1, :job nil}} 3)
#_(update-workers {0 {:until 3, :job "C"}, 1 {:until 4, :job "D"}, 2 {:until -1, :job nil}} 3)
(make-workers 3)
;; => {0 {:until 3, :job "C"}, 1 {:until -1, :job nil}, 2 {:until -1, :job nil}}
;; (update-workers (make-workers 2) 4)
;; => {:workers {0 {:until -1, :job nil}, 1 {:until -1, :job nil}}, :finished-jobs ()}
(defn is-all-workers-idle? [workers]
  (every? (fn [[k v]]
            (nil? (:job v)))
          workers))
#_(is-all-workers-idle? (make-workers 3))

(defn allocate-jobs-to-workers [workers jobs sec]
  (let [sorted-jobs       (sort jobs)
        free-workers      (->> workers
                               (filter (fn [[_ k]] (not (:job k))))
                               (map first))
        ;a   (prn free-workers)
        allocated-job     (zipmap free-workers sorted-jobs)
        allocated-job-cnt (count allocated-job)
        remaining-job     (drop allocated-job-cnt sorted-jobs)

        allocated-job-and-worker
        (reduce (fn [acc [worker job]]
                  (conj acc {worker {:job   job
                                     :until (+ sec (time-to-finish job))}}))
                {}
                allocated-job)]
    {:workers (reduce
               (fn [acc [k v]]
                 (assoc acc k v))
               workers
               allocated-job-and-worker)
     :remaining-jobs remaining-job}))

(allocate-jobs-to-workers {0 {:until -1, :job nil}, 1 {:until 3, :job "C"}} (list "D" "E") 10)
(allocate-jobs-to-workers {0 {:until 4, :job "Z"}, 1 {:until 3, :job "C"}} (list "D" "E") 10)

;; allocate-jobs-to-workers 에서 잡을 할당함.
;; 할당되지 않은 녀석들은?
(defn top-sort-with-workers [graph workers]
  (loop [g graph
         no-deps-items_ (into '() (no-deps-items g)) ;; lazy-seq
         res '()
         workers' workers #_(:workers (allocate-jobs-to-workers workers no-deps-items_ 0))
         sec 0]
    #_(prn [sec res])
    #_(prn [sec "before-update " no-deps-items_])
    #_(prn workers')
    (let [updated-workers (update-workers workers' sec)    ;; worker 갱신
          ;a (prn updated-workers)
          finished-jobs (->> (:finished-jobs updated-workers)
                             (map (fn [e] (:job e))))   ;; 끝난 job
          removed (remove-item-from-deps2 g finished-jobs)  ;; 끝난 job을 deps에서 제거
          no-deps-items_' (sort (concat no-deps-items_ (:no-deps removed))) ;; 남은 deps와 끝난 job들을 모음.
          allocated-jobs-to-workers (allocate-jobs-to-workers (:workers updated-workers) no-deps-items_' sec)  ;; 남은 deps를 재할당
          new-workers (:workers allocated-jobs-to-workers) ;; 일하는 workers
          remaining-jobs (:remaining-jobs allocated-jobs-to-workers)]  ;; 남은 job들
      #_(prn [sec "workers " updated-workers])
      #_(prn [sec "graph " removed])
      #_(prn [sec "no-deps " no-deps-items_'])

      #_(prn [sec updated-workers])
      #_(prn [sec removed])
      #_(prn [sec "allocated " allocated-jobs-to-workers])
      #_(prn [sec remaining-jobs])
      #_(prn [sec no-deps-items_'])
      #_(prn [sec allocated-jobs-to-workers])
      #_(prn updated-workers)
      #_(prn res finished-jobs)
      #_(prn ["allocated-jobs-to-workers " allocated-jobs-to-workers])
      (if (or #_(< 1000 sec) (is-all-workers-idle? new-workers))
        [sec (concat res finished-jobs)]
        (recur (:remaining-vertexes removed)
               remaining-jobs
               (concat res finished-jobs)
               new-workers
               (inc sec))))))
(top-sort-with-workers demo-input (make-workers 1))
(top-sort-with-workers real-input (make-workers 5))
(let [res (top-sort-with-workers real-input (make-workers 5))]
  (->> (second res )
       
       (apply str)))
(->> (first (top-sort-with-workers real-input (make-workers 5))))

real-input
;; 파트원 답안 SCLPAMQVUWNHODRTGYKBJEFXZI 성공
;; 파트투 시도 SCLPAMQVWYNUHODTRGKBJEFXZI 실패
;; 214 too low
;; 1001 too low 
;; 1234 correct