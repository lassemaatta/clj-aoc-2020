(ns aoc-2020.day3
  (:require [clojure.string :as str]))

(def input-map {\. false
                \# true})

(defn- input->area
  [input]
  (->> input
       (str/split-lines)
       (mapv (fn [line]
               (mapv input-map line)))))

(defn tree?
  [area x y]
  (let [width (count (first area))
        x1    (mod x width)]
    (get-in area [y x1])))

(defn path
  [^long step-x ^long step-y]
  (->> (range)
       (map (fn [^long v] [(* step-x v)
                           (* step-y v)]))))

(defn make-path
  [area [step-x step-y]]
  (let [height (count area)]
    (->> (path step-x step-y)
         (take height))))

(defn count-trees
  [area path]
  (->> path
       (filter (fn [loc] (apply tree? area loc)))
       (count)))

(defn problem-1
  [data]
  (let [area (input->area data)
        path (make-path area [3 1])]
    (count-trees area path)))

(defn problem-2
  [data]
  (let [area   (input->area data)
        slopes [[1 1]
                [3 1]
                [5 1]
                [7 1]
                [1 2]]
        trees  (->> slopes
                    (map #(make-path area %))
                    (map #(count-trees area %)))]
    (apply * trees)))
