(ns aoclj2020)

(def regex #"(\d+)-(\d+) ([a-zA-Z]): ([a-zA-Z]+)")

(def test-input ["1-3 a: abcde"
                 "1-3 b: cdefg"
                 "2-9 c: ccccccccc"])

(def input (clojure.string/split-lines (slurp "day2.input")))

(defn match [s] (re-matcher regex s))

(defn str->policy [s]
  (let [matches (re-find (match s))
        least (second matches)
        most (nth matches 2)
        letter (nth matches 3)
        pw (nth matches 4)]
    [(Integer/parseInt least) (Integer/parseInt most) letter pw]))


(defn test-policy [[least most letter password]]
  (let [freqs (frequencies (map str password))]
    (<= least (or
               (freqs letter) 0)
        most)))

(defn solve1 [inp]
    (->> inp
        (map str->policy)
        (filter test-policy)
        count))

(assert (= 2 (solve1 test-input)))

(solve1 input)

(defn xor [x y]
  (or (and (not x) y) (and x (not y))))

(defn test-policy2 [[least most letter password]]
  (let [l (str (get password (dec least)))
        m (str (get password (dec most)))]
    (xor (= l letter) (= m letter))))

(defn solve2 [inp]
  (->> inp
       (map str->policy)
       (filter test-policy2)
       count))

(solve2 input)
