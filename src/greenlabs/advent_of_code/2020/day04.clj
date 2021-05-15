(ns greenlabs.advent-of-code.2020.day04
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [clojure.java.io :as io]))

;;  part one
;; 필수적인 fields가 없으면 invalid
;; 비필수적인 cid가 없는것은 ok
(defn data-to-map [str]
  (walk/keywordize-keys (apply hash-map
                               (str/split str #":|\s+"))))

(defn isvalid-password? 
  "지정한 키가 존재하는지 모두 존재하면 true를 리턴합니다."
  [input]
  (let [properties [:byr :iyr :eyr :hgt :hcl :ecl :pid]]
    (every? input properties)))
#_(isvalid-password? {:byr 1 :iyr 1 :eyr 1 :hgt 1 :hcl 1 :ecl 1 :pid 1})



;; part one
#_(defn part-one [input]
  (->> input
       (map data-to-map)
       (filter isvalid-password?)
       count))

#_(part-one (str/split (slurp (io/resource "2020/day04.txt")) #"\n{2}"))
;; 202

;; part two
;; byr (Birth Year) - 숫자 4개; 1920 <= x <= 2002
;; iyr (Issue Year) - 숫자 4개; 2010 <= x <= 2020
;; eyr (Expiration Year) - 숫자 4개; 2020 <= x <= 2030
;; hgt (Height) - 숫자[cm|in]
;; cm인 경우,150 <= x <= 193
;; in인 경우,59 <= x <= 76
;; hcl (Hair Color) - [0-9a-f]{6}
;; ecl (Eye Color) - 다음 중 하나 amb blu brn gry grn hzl oth.
;; pid (Passport ID) - 9개 숫자 0으로 시작해도 됨
;; cid (Country ID) - 무시 그냥 패스 

;;  part two
;;  valid
(defmulti
  passport-valid? first)

(defn to-digit
  "str가 숫자라면 int형으로 변환하여 리턴합니다. 숫자가 아니라면 nil을 리턴합니다."
  [str]
  (if-let [digit-str (re-matches #"^\d+" str)]
    (Integer/parseInt digit-str)))

(defmethod passport-valid?
  :byr
  [[_ v]]
  (if-let [digit (to-digit v)]
    (<= 1920 digit 2002)))

(defmethod passport-valid?
  :iyr
  [[_ v]]
  (if-let [digit (to-digit v)]
    (<= 2010 digit 2020)))

(defmethod passport-valid?
  :eyr
  [[_ v]]
  (if-let [digit (to-digit v)]
    (<= 2020 digit 2030)))

(defmethod passport-valid?
  :hgt
  [[_ v]]
  (let [hgt (re-find #"(\d+)(\w+)" v)
        [_ value unit] hgt]
    (case unit
      "cm" (as-> value $
             (Integer/parseInt $)
             (<= 150 $ 193))
      "in" (<= 59 (Integer/parseInt value) 76)
      false)))

(defmethod passport-valid?
  :hcl
  [[_ v]]
  (boolean (re-matches #"^#[0-9a-f]{6}$" v)))

(defmethod passport-valid?
  :ecl
  [[_ v]]
  (contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}
             v))

(defmethod passport-valid?
  :pid
  [[_ v]]
  (boolean (re-matches #"^\d{9}$" v)))

(defmethod passport-valid?
  :cid [_] true)

(defmethod passport-valid?
  :default [_] true)

(defn valid-pass [pass]
  (when (every? identity (map passport-valid? pass))
    pass))
;; resource 1
;; https://www.cs.cmu.edu/Groups/AI/html/faqs/lang/lisp/part1/faq-doc-4.html
;; Never use a 2-argument IF or a 3-argument IF with a second
;;         argument of NIL unless you want to emphasize the return value;
;;         use WHEN and UNLESS instead. You will want to emphasize the
;;         return value when the IF clause is embedded within a SETQ,
;;         such as (SETQ X (IF (EQ Y Z) 2 NIL)). If the second argument 
;;         to IF is the same as the first, use OR instead: (OR P Q) rather
;;         than (IF P P Q). Use UNLESS instead of (WHEN (NOT ..) ..)
;;         but not instead of (WHEN (NULL ..) ..).
;;       - Use COND instead of nested IF statements. Be sure to check for
;;         unreachable cases, and eliminate those cond-clauses.

;; resource 0
;; https://stuartsierra.com/2015/06/16/clojure-donts-single-branch-if 
;; Some people like the third variant, if with no “else” branch, 
;; because they think when is only for side-effects, leaving the single-branch if for “pure” code.
;; But for me it comes down, as usual, to readability.
;; ... 

;; ps-contained-essential-keys 가 새로운 타입을 가져야 하는 것?
;; ps-fully-validated도 새로운 타입이 필요한지?
;; 예외를 던지기 위해서 :pre :post를 써야하나?
(let [passports-str (str/split (slurp (io/resource "2020/day04.txt"))  #"\n{2}")
      passports-map (map data-to-map passports-str)
      ps-contained-essential-keys (filter isvalid-password? passports-map)
      ps-fully-validated (keep valid-pass ps-contained-essential-keys)]
  {:part-one (count ps-contained-essential-keys)
   :part-two (count ps-fully-validated)})