(ns aoc-2020.day10
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:import (clojure.lang PersistentQueue)))

(def sample-data-1 "16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4")

(def sample-data-2 "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3")

(def queue PersistentQueue/EMPTY)

; From JoC
(defmethod print-method PersistentQueue [q, w]
  (print-method '<- w)
  (print-method (seq q) w)
  (print-method '-< w))

(defn- input->vals
  [input]
  (->> input
       (str/split-lines)
       (mapv #(Long/parseLong %))))

(defn- build-chain
  [vals]
  (let [sorted  (sort vals)
        adapter (+ 3 ^long (apply max sorted))]
    (concat [0] sorted [adapter])))

(defn calculate-differences
  [vals]
  (let [chain (build-chain vals)]
    (->> chain
         (partition 2 1)
         (map (fn [[^long a ^long b]] (- b a)))
         (frequencies))))

(defn task-1
  [data]
  (let [freqs (->> (input->vals data)
                   (calculate-differences))]
    (* ^long (get freqs 1)
       ^long (get freqs 3))))

(defn reachable?
  [^long source ^long target]
  (>= (+ 3 source) target))

(defn sum-paths
  [nbors target]
  (->> nbors
       (filter #(reachable? (:val %) target))
       (map :paths)
       (apply +)))

(defn traverse
  [q x]
  (let [nbors  (seq q)
        paths  (sum-paths nbors x)
        next-q (conj q {:val   x
                        :paths paths})]
    (if (< 4 (count next-q))
      (pop next-q)
      next-q)))

(defn calculate-paths
  [data]
  (let [chain (->> (input->vals data)
                   (build-chain))]
    (->> (rest chain)
         (reduce
           traverse
           (conj queue {:val   (first chain)
                        :paths 1}))
         (last)
         (:paths))))

(comment
  (->> (input->vals sample-data-1)
       (calculate-differences))
  (->> (input->vals sample-data-2)
       (calculate-differences))
  (let [data (slurp (io/resource "day10.txt"))]
    (task-1 data)
    (calculate-paths data)))
