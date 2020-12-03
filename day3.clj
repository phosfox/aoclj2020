(ns aoclj2020
  (:require [clojure.string :as s]))

(def input
  (->> "day3.input"
       slurp
       s/split-lines))
       
(def input-count (count input))

(print input-count)


(defn slope->steps [[right down]]
  (let [downs (range down input-count down)
        rights (iterate #(mod (+ right %) 31) right)]
    (map vector downs rights)))

(def steps
  (let [rights (iterate #(mod (+ 3 %) 31) 3)
        downs (range 1 input-count)]
    (map vector downs rights)))

(defn solve1 [steps]
  (->> steps
    (slope->steps)
    (map #(get-in input %))
    (filter #(= \# %))
    count)) 

(solve1 [3 1])

(defn solve2 []
  (let [a (solve1 [1 1])
        b (solve1 [3 1])
        c (solve1 [5 1])
        d (solve1 [7 1])
        e (solve1 [1 2])]
    (* a b c d e)))

(solve2)
