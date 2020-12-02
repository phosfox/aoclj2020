(ns aoclj2020)

(def test-input [1721 979 366 299 675 1456])

(defn parse-int [n]
  (Integer/parseInt n))

(def raw-input (clojure.string/split-lines
                (slurp "day1.input")))
(first raw-input)

(def input (map parse-int raw-input))

(defn solve1 [nn]
  (first
    (for [n nn
            b (rest nn)
            :when (= 2020 (+ n b))]
        (* b n))))

(defn solve2 [nn]
  (first
   (for [n nn
            b (rest nn)
            c (drop 2 nn)
            :when (= 2020 (+ n b c))]
        (* b c n))))

(assert (= 514579
           (solve1 test-input)))

(assert (= 241861950
           (solve2 test-input)))

(solve1 input)
(solve2 input)
