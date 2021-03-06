(ns aoclj2020
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def test-input
  ["BFFFBBFRRR"
   "FFFBBBFRRR"
   "BBFFBBFRLL"])

(def real-input
  (str/split-lines (slurp "day5.input")))

(def rows (range 128))

(def columns (range 8))

(def seat-code {\F first \B second \L first \R second})

(defn binary-partition [rows]
  (partition (/ (count rows) 2) rows))

(defn find-seat [pos ss]
  (reduce get-halve pos ss))

(defn get-halve [row ch]
  (let [part-r (binary-partition row)]
    ((seat-code ch) part-r)))

(defn seat-id [ss]
  (let [matches (re-find #"(\w{7})(\w{3})" ss)
        r (second matches)
        c (last matches)
        row-id (find-seat rows r)
        col-id (find-seat columns c)]
    (+ (* 8 (first row-id)) (first col-id))))

(defn solve1 [inp]
  (->>
    inp
    (map seat-id)
    (apply max)))

(defn solve2 [inp]
  (let [taken (sort (map seat-id inp))]
    (set/difference (set (range (first taken) (last taken)))
                    (set taken))))

(solve1 real-input)
(solve2 real-input)


(set/difference #{1 2 3 4} #{1 2 3 5})
