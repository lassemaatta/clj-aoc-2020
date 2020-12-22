(ns aoc-2020.day10
  (:require [clojure.string :as str])
  (:import (clojure.lang PersistentQueue)))

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
