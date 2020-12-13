(ns aoc-2020.day9
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:import (clojure.lang PersistentQueue)))

(def sample-data "35\n20\n15\n25\n47\n40\n62\n55\n65\n95\n102\n117\n150\n182\n127\n219\n299\n277\n309\n576")

(set! *warn-on-reflection* true)

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

(defn find-abnormal
  [values len]
  (reduce
    (fn [q v]
      (if (< (count q) len)
        (conj q v)
        (let [candidates (sort q)
              pairs      (for [x candidates
                               y candidates
                               :while (> x y)
                               :when (= v (+ x y))]
                           [x y])]
          (if (seq pairs)
            (-> q
                (conj v)
                (pop))
            (reduced v)))))
    queue
    values))

(defn find-results
  [values sum]
  (let [cnt (count values)]
    (for [start (range cnt)
          end   (range (inc start) cnt)
          :let [scope  (subvec values start end)
                result (apply + scope)]
          :while (and (>= end start)
                      (<= result sum))
          :when (= sum result)]
      {:start start
       :end   end
       :min   (apply min scope)
       :max   (apply max scope)})))

(defn find-sum
  [values sum]
  (->> (find-results values sum)
       (take 1)
       (map (fn [{:keys [min max]}]
              [min max]))
       (first)
       (apply +)))

(comment
  (let [data sample-data
        len  5]
    [(-> (input->vals data)
         (find-abnormal len))
     (-> (input->vals data)
         (find-sum 127))])
  (let [data (slurp (io/resource "day9.txt"))
        len  25]
    [(-> (input->vals data)
         (find-abnormal len))                               ;
     (-> (input->vals data)
         (find-sum 1504371145))]))
