(ns aoc-2020.day1
  (:require [clojure.string :as str]))

(defn- input->nums
  [input]
  (->> input
       (str/split-lines)
       (map (fn [v] (Long/parseLong v)))
       (sort)))

(defn find-two-entries
  [data]
  (let [vals    (input->nums data)
        matches (for [^long x vals
                      ^long y vals
                      :while (> x y)
                      :when (= 2020 (+ x y))]
                  [x y])]
    (->> matches
         (map (fn [[^long x ^long y]] (* x y)))
         (first))))

(defn find-three-entries
  [data]
  (let [vals    (input->nums data)
        matches (for [^long x vals
                      ^long y vals
                      ^long z vals
                      :while (> x y z)
                      :when (= 2020 (+ x y z))]
                  [x y z])]
    (->> matches
         (map (fn [[^long x ^long y ^long z]] (* x y z)))
         (first))))
