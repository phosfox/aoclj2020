(ns aoclj2020
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def test-input
  "abc

a
b
c

ab
ac

a
a
a
a

b")

(def real-input (slurp "day6.input"))

(defn solve1 [inp]
  (->> (str/split inp #"\R\R")
     (map #(str/replace % #"\n" ""))
     (map set)
     (map count)
     (reduce +)))

(defn solve2 [inp]
  (->> (str/split inp #"\R\R")
       (map str/split-lines)
       (map #(map set %))
       (map #(apply set/intersection %))
       (map count)
       (reduce +)))

(solve1 real-input)
(solve2 real-input)
