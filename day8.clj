(ns aoclj2020
  (:require [clojure.string :as str]))

(def test-input
  "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")

(def real-input (slurp "day8.input"))

(defn parse-arg [instr]
 (update instr 1 #(Long/parseLong %)))

(defn parse-instructions [input]
  (->> input
     str/split-lines
     (map #(re-find #"(\w+)\s([\+\-]\d+)" %))
     (mapv (comp parse-arg vec rest))))

(def instructions (parse-instructions test-input))

(do instructions)

(defn seen? [col v]
  (col v))

(seen? #{["acc" 2] ["nop" 3]} ["acc" 2])

(defn run [instr]
  (loop [acc 0
         pos 0
         seen #{}]
    (let [[opp val :as all] (get instr pos)]
     (if  (seen? seen pos)
       acc
       (case opp
         "acc" (recur (+ acc val) (inc pos) (conj seen pos))
         "jmp" (recur acc (+ pos val) (conj seen pos))
         "nop" (recur acc (inc pos) (conj seen pos)))))))

(run (parse-instructions test-input))

(defn run2 [instr]
  (loop [acc 0
         pos 0
         seen #{}]
    (let [[opp val :as all] (get instr pos)]
      (cond
        (seen? seen pos) :loop
        (= (count instr) pos) acc
        :else
        (case opp
          "acc" (recur (+ acc val) (inc pos) (conj seen pos))
          "jmp" (recur acc (+ pos val) (conj seen pos))
          "nop" (recur acc (inc pos) (conj seen pos)))))))

(let [program (parse-instructions real-input)]
  (for [i (range (count program))
        :when (#{"nop" "jmp"} (get-in program [i 0]))
        :let [new-prog (update-in program [i 0] {"nop" "jmp" "jmp" "nop"})
              result (run2 new-prog)]
        :when (not= result :loop)]
    result))
