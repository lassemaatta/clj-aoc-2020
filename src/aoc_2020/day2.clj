(ns aoc-2020.day2
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]))

(s/def ::min number?)
(s/def ::max number?)
(s/def ::char char?)
(s/def ::password string?)

(s/def ::row (s/keys :req [::min
                           ::max
                           ::char
                           ::password]))

(defn policy-1-password?
  [{::keys [min max char password]}]
  (let [num (-> (frequencies password)
                (get char 0))]
    (<= min num max)))

(s/def ::valid-policy-1-row (s/and ::row
                                   policy-1-password?))

(defn policy-2-password?
  [{::keys [^long min ^long max char password]}]
  (let [a (= char (get password (dec min)))
        b (= char (get password (dec max)))]
    (not= a b)))

(s/def ::valid-policy-2-row (s/and ::row
                                   policy-2-password?))
(defn- input->fields
  [input]
  (->> input
       (str/split-lines)
       (map (fn [line]
              (let [[_ min max char password] (re-find #"(\d+)-(\d+) (\w): (\w+)" line)]
                {::min      (Long/parseLong min)
                 ::max      (Long/parseLong max)
                 ::char     (first char)
                 ::password password})))))

(defn count-valid
  [input policy]
  (let [rows      (input->fields input)
        is-valid? #(s/valid? policy %)]
    (count (filter is-valid? rows))))
