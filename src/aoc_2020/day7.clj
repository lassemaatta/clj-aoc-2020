(ns aoc-2020.day7
  (:require [clojure.string :as str]))

(defn- parse-contents
  [contents]
  (->> contents
       (filter #(not= "no other bags" %))
       (map #(re-find #"(\d+) (\w+ \w+) bag[s]*" %))
       (map (fn [[_ count bag]]
              [bag (Long/parseLong count)]))
       (into {})))

(defn parse-graph
  [data]
  (->> data
       (str/split-lines)
       (map #(str/split % #" bags contain |, |\."))
       (filter (comp seq first))
       (map (fn [[bag & contents]]
              [bag (parse-contents contents)]))
       (into {})))

(defn next-nodes
  [g node]
  (keys (get g node)))

(defn reverse-edges
  [edges]
  (mapv (fn [[src dst ^long cnt]]
          [dst src (/ 1 cnt)])
        edges))

(defn graph->edges
  [g]
  (->> g
       (mapcat (fn [[parent contents]]
                 (->> contents
                      (map (fn [[bag count]]
                             [parent bag count])))))))

(defn edges->graph
  [edges]
  (reduce
    (fn [k [src dst cnt]]
      (assoc-in k [src dst] cnt))
    {}
    edges))

(defn reverse-graph
  [graph]
  (-> graph
      (graph->edges)
      (reverse-edges)
      (edges->graph)))

(defn- find-nodes
  [graph node]
  (loop [node          node
         traverse-next #{}
         parents       #{}]
    (let [our-predecessors (next-nodes graph node)]
      (if (nil? our-predecessors)
        (if (empty? traverse-next)
          parents
          (recur (first traverse-next)
                 (rest traverse-next)
                 parents))
        (let [next-nodes (into (or traverse-next #{}) our-predecessors)]
          (recur (first next-nodes)
                 (rest next-nodes)
                 (into parents our-predecessors)))))))

(defn count-parents
  [data node]
  (-> (parse-graph data)
      (reverse-graph)
      (find-nodes node)
      (count)))

(defn count-children*
  ^long [g node]
  (let [children (get g node)]
    (if (empty? children)
      1
      (let [^long child-count (reduce
                                (fn [^long sum [node ^long cnt]]
                                  (+ sum (* cnt (count-children* g node))))
                                0
                                children)]
        (inc child-count)))))

(defn count-children
  [data node]
  (-> (parse-graph data)
      (count-children* node)
      (dec)))
