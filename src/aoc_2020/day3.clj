(ns aoc-2020.day3
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def sample-data "..##.......\n#...#...#..\n.#....#..#.\n..#.#...#.#\n.#...##..#.\n..#.##.....\n.#.#.#....#\n.#........#\n#.##...#...\n#...##....#\n.#..#...#.#")

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
  [step-x step-y]
  (->> (range)
       (map (fn [v] [(* step-x v)
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

(comment
  (problem-1 sample-data)
  (problem-1 (slurp (io/resource "day3.txt")))

  (problem-2 sample-data)
  (problem-2 (slurp (io/resource "day3.txt"))))
