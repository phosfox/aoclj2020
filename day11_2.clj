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
            {cell (new-seat world cell)}))))

(defn next-generation2 [world]
  (let [cells (keys world)
        old-world world]
    (loop [world world
           cells cells]
      (if (seq cells)
        (let [cell (first cells)]
          (recur (assoc world cell (new-seat old-world cell)) (next cells)))
        world))))

(defn new-seat [world cell]
  (let [seat (get world cell)]
    (case seat
      \L (if (= 0 (occupied-neighbours world cell)) \O \L)
      \O (if (>= (occupied-neighbours world cell) 4) \L \O))))

(defn next-generation3 [world]
  (let [cells (keys world)
        old-world world]
   (reduce
    (fn [new-world cell]
     (let [nbs (neighbours cell)]
       (assoc new-world cell (new-seat old-world cell))))
    world
    cells)))
  
            
(time (next-generation world))
(time (next-generation2 world))
(time (next-generation3 world))

(=
 (next-generation world)
 (next-generation2 world)
 (next-generation3 world))

(defn count-occupied [world]
  (->> world
       vals
       (filter #(= \O %))
       count))

(defn find-stable-generation [world new-gen-fn]
  (reduce
   (fn [old-gen next-gen]
     (if (= old-gen next-gen)
       (reduced old-gen)
       next-gen))
   {}
   (iterate #(new-gen-fn %) world)))

(def solution1
  (count-occupied (find-stable-generation (parse-world test-input))))

(do solution1)

(time (count-occupied (find-stable-generation (parse-world real-input) next-generation)))
(time (count-occupied (find-stable-generation (parse-world real-input) next-generation2)))
(time (count-occupied (find-stable-generation (parse-world real-input) next-generation3)))
