(ns aoc-2020.day9
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

(defn find-abnormal
  [values ^long len]
  (reduce
    (fn [q v]
      (if (< (count q) len)
        (conj q v)
        (let [candidates (sort q)
              pairs      (for [^long x candidates
                               ^long y candidates
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
  [values ^long sum]
  (let [cnt (count values)]
    (for [^long start (range cnt)
          ^long end   (range (inc start) cnt)
          :let [scope        (subvec values start end)
                ^long result (apply + scope)]
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

(defn problem-1
  [data len]
  (-> (input->vals data)
      (find-abnormal len)))

(defn problem-2
  [data target]
  (-> (input->vals data)
      (find-sum target)))
