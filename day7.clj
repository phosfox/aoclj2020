(ns aoclj2020
  (:require [clojure.string :as str]))

(def test-input (str/split-lines
                 (slurp "day7-test.input")))

(def real-input (str/split-lines
                 (slurp "day7.input")))

(defn parse-bag [s]
  (let [[bag & rest] (str/split s #"\s?(contain|,)\s?")
        color (re-find #"\w+ \w+" bag)
        desp (map next
                (map #(re-find #"(\d+) (\w+ \w+)" %) rest))]
    [color desp]))

(defn bag-graph [inp]
  (reduce (fn [m [bag deps]]
            (reduce (fn [m [num col]]
                      (update m col conj bag))
                    m
                    deps))
          {}
          inp))

(def graph (bag-graph (map parse-bag test-input)))

(defn find-bags [graph start]
  (loop [res (into #{} (get graph start))]
    (let [res2 (reduce (fn [res color]
                         (into res (get graph color)))
                       res
                       res)]
      (if (= res res2)
        res
        (recur res2)))))

(def graph
  {"light red" ["bright white" "muted yellow"]
   "dark orange" ["bright white" "muted yellow"]
   "bright white" ["shiny gold"]
   "muted yello" ["shiny gold" "faded blue"]
   "shiny gold" ["dark olive" "vibrant plum"]
   "dark olive" ["faded blue" "dotted black"]
   "vibrant plum" ["faded blue" "dotted black"]
   "faded blue" []
   "dotted black" []})

(defn visited?
  "Predicate which returns true if the node v has been visited already, false otherwise."
  [v coll]
  (some #(= % v) coll))

(defn find-neighbors
  "Returns the sequence of neighbors for the given node"
  [v coll]
  (get coll v))

(defn graph-bfs
  "Traverses a graph in Breadth First Search (BFS)."
  [graph v]
  (loop [queue   (conj clojure.lang.PersistentQueue/EMPTY v) ;; Use a queue to store the nodes we need to explore
         visited #{}]                                         ;; A vector to store the sequence of visited nodes
    (if (empty? queue) visited                               ;; Base case - return visited nodes if the queue is empty
        (let [v           (peek queue)
              neighbors   (find-neighbors v graph)
              not-visited (filter (complement #(visited? % visited)) neighbors)
              new-queue   (apply conj (pop queue) not-visited)]
          (if (visited? v visited)
            (recur new-queue visited)
            (recur new-queue (conj visited v)))))))

(count
 (disj
  (graph-bfs (bag-graph (map parse-bag real-input)) "shiny gold") "shiny gold"))


(defn bag-graph-nums [inp]
  (reduce (fn [m [bag deps]]
            (reduce (fn [m [num col]]
                      (let [new-deps (if (nil? num) [] [(Long/parseLong num) col])]
                        (update m bag conj new-deps)))
                    m
                    deps))
          {}
          inp))


(defn graph-dfs
  "Traverses a graph in Breadth First Search (BFS)."
  [graph v]
  (loop [stack   (vector v) ;; Use a stack to store the nodes we need to explore
         visited []
         acc 1]
    (if (empty? stack) [visited acc]                               ;; Base case - return visited nodes if the queue is empty
        (let [v     (peek stack)
              neighbors   (find-neighbors v graph)
              not-visited (filter (complement #(visited? % visited)) (mapcat next neighbors))
              new-stack   (apply conj (pop stack) not-visited)
              nums (apply * (filter number? (map first neighbors)))]
          (if (visited? v visited)
            (recur new-stack visited acc)
            (recur new-stack (conj visited v) (+ acc nums)))))))

(defn parse-entry [entry]
  (let [p  (re-find #"\w+ \w+" entry)
        cs (->> (re-seq #"(\d+) (\w+ \w+)" entry)
                (reduce (fn [m [_ v k]] (assoc m k (Long/parseLong v))) {}))]
    [p cs]))

(def bags (into {} (map parse-entry test-input)))

(do bags)

(defn find-outer-bags [bags bag]
  (let [found-bags (keys (filter #(contains? (val %) bag) bags))]
    (into (set found-bags) (mapcat #(find-outer-bags bags %) found-bags))))

(count (find-outer-bags bags "shiny gold"))

(defn count-bags [bags [bag n]]
  (* n (apply + 1 (map #(count-bags bags %) (get bags bag)))))

(dec (count-bags bags ["shiny gold" 1])) 
