(ns aoclj2020
  (:require [clojure.string :as str]))

(def test-input
  (->> "day11_demo.input"
       slurp
       str/split-lines))

(def real-input
  (->> "day11.input"
       slurp
       str/split-lines))

(defn parse-world [input]
  (let [row-max (count input)
        col-max (count (first input))]
    (into {}
          (for [row (range row-max)
                col (range col-max)
                :let [seat (get-in input [row col])]
                :when (not= seat \.)]
            {[row col] seat}))))

(defn neighbours [[row col]]
  (for [r (range -1 2)
        c (range -1 2)
        :when (not (and (= 0 r) (= 0 c)))]
    [(+ row r) (+ col c)]))

(neighbours [2 4])

(defn occupied-neighbours [world [row col]]
  (let [nbs (neighbours [row col])]
    (count (filter #(= \O %) (map #(get world %) nbs)))))

(occupied-neighbours world [2 4])

(defn next-generation [world]
  (let [cells (keys world)]
    (into {}
          (for [cell cells
                :let [nbs (neighbours cell)
                      seat (get world cell)]]
            (case seat
              \L (if (= 0 (occupied-neighbours world cell)) {cell \O} {cell seat})
              \O (if (>= (occupied-neighbours world cell) 4) {cell \L} {cell seat})
              :else {cell seat})))))

(defn count-occupied [world]
  (->> world
       vals
       (filter #(= \O %))
       count))

(defn find-stable-generation [world]
  (reduce
   (fn [old-gen next-gen]
     (if (= old-gen next-gen)
       (reduced old-gen)
       next-gen))
   {}
   (iterate #(next-generation %) world)))

(def solution1
  (count-occupied (find-stable-generation (parse-world real-input))))
