(ns aoclj2020
  (:require [clojure.walk :as walk]))

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

(defn bfs [graph start result]
  (if (some? start)
    (let [vals (get graph start)
          new-res (into result vals)]
      (bfs graph (first vals) new-res))
    result))


(bfs graph "shiny gold" #{})

(apply #(bfs graph % #{}) ["dark olive" "vibrant plum"])

(into #{} [:a :b] [:c])

(conj [1 2] 3)
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
         visited []]                                         ;; A vector to store the sequence of visited nodes
    (if (empty? queue) visited                               ;; Base case - return visited nodes if the queue is empty
        (let [v           (peek queue)
              neighbors   (find-neighbors v graph)
              not-visited (filter (complement #(visited? % visited)) neighbors)
              new-queue   (apply conj (pop queue) not-visited)]
          (if (visited? v visited)
            (recur new-queue visited)
            (recur new-queue (conj visited v)))))))

(graph-bfs graph "shiny gold")

clojure.lang.PersistentQueue/EMPTY
