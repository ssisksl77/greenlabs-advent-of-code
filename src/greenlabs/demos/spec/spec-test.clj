(ns greenlabs.demos.spec.spec-test
  (:require [clojure.spec.alpha :as s]))


(s/conform even? 1000)
(s/valid? even? 10)

(s/valid? #{:club :diamond :heart :spade} :club) ;; true

;; 재사용하기

(s/def ::date inst?)
(s/def ::suit #{:club :diamond :heart :spade})

(s/valid? ::date (java.util.Date.))
(s/conform ::suit :club)


