(ns aoclj2020
  (:require [clojure.string :as s]))

(def input
  (->> "day3.input"
       slurp
       s/split-lines))
       
(def input-count (count input))

(def steps
  (let [rights (iterate #(mod (+ 3 %) 32) 2)
        downs (range 1 input-count)]
    (map vector downs rights)))

(take 5 steps)


(->> steps
    (map #(get-in input %))
    (take 10))
(filter #(= \# %))
count
              
