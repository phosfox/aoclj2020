(ns aoclj2020
  (:require [clojure.string :as str]))

(def test-input [35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576]) 

(def test-nums (drop 5 test-input))

(def real-input (->> "day9.input"
                     slurp
                     str/split-lines
                     (map #(Long/parseLong %))))

(def real-nums (drop 25 real-input))

(defn combs [col]
  (set
   (for [i col
         k col]
    (+ i k)))) 

(defn all-combs [inp]
  (->> inp
     (partition 25 1)
     (map combs)))

(defn find-invalid [nums combs]
  (for [[c n] (map vector combs nums)
        :when (not (contains? c n))]
    n))

(find-invalid real-nums (all-combs real-input))
