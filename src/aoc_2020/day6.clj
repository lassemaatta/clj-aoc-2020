(ns aoc-2020.day6
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def empty-pending {:rows []})

(defn commit-pending
  [{:keys [pending] :as m}]
  (let [rows    (:rows pending)
        pending (-> pending
                    (assoc :any (apply set/union rows))
                    (assoc :all (apply set/intersection rows)))]
    (-> m
        (update :answers conj pending)
        (assoc :pending empty-pending))))

(defn collect-field
  [m field]
  (update-in m [:pending :rows] conj (set field)))

(defn process-field
  [m field]
  (if (empty? field)
    (commit-pending m)
    (collect-field m field)))

(defn- input->answers
  [input]
  (->> (str/split input #"\s")
       (map seq)
       (reduce
         process-field
         {:answers []
          :pending empty-pending})
       (commit-pending)
       (:answers)))

(defn- sum-answer-counts
  [selector answers]
  (->> answers
       (map selector)
       (filter seq)
       (map count)
       (apply +)))

(defn parse-and-count
  [input selector]
  (->> input
       (input->answers)
       (sum-answer-counts selector)))
