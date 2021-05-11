(ns greenlabs.advent-of-code.util
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn slurp-resource-lines [path]
  (let [lines (slurp (io/resource path))]
    (str/split-lines lines)))

