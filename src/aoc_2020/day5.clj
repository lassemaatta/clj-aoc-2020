(ns aoc-2020.day5
  (:require [clojure.string :as str]))

(def token->bit {\B "1"
                 \F "0"
                 \R "1"
                 \L "0"})

(defn- input->seats
  [input]
  (->> input
       (str/split-lines)
       (mapv (fn [line]
               (let [as-string (->> line
                                    (map token->bit)
                                    (apply str))
                     v         (Long/parseLong as-string 2)
                     column    (bit-and v 2r111)
                     row       (bit-shift-right v 3)]
                 {:row    row
                  :column column
                  :seat   v})))))

(defn max-seat
  [data]
  (let [seats (input->seats data)]
    (->> seats
         (map :seat)
         (apply max))))

(defn find-seat
  [data]
  (let [seats (input->seats data)]
    (->> seats
         (map :seat)
         (sort)
         (reduce
           (fn [^long previous ^long seat]
             (if (= (+ previous 2) seat)
               (reduced (dec seat))
               seat))
           -1))))
