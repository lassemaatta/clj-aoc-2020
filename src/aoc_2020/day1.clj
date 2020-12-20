(ns aoc-2020.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def sample-data "1721\n979\n366\n299\n675\n1456")

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
    (mapv (fn [[^long x ^long y]] (* x y)) matches)))

(defn find-three-entries
  [data]
  (let [vals    (input->nums data)
        matches (for [^long x vals
                      ^long y vals
                      ^long z vals
                      :while (> x y z)
                      :when (= 2020 (+ x y z))]
                  [x y z])]
    (mapv (fn [[^long x ^long y ^long z]] (* x y z)) matches)))

(comment
  (find-two-entries sample-data)
  (find-two-entries (slurp (io/resource "day1.txt")))

  (find-three-entries sample-data)
  (find-three-entries (slurp (io/resource "day1.txt"))))
