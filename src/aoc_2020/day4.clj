(ns aoc-2020.day4
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]))

(def sample-data "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\nhcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm\n\nhcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in")

(s/def ::byr-simple string?)
(s/def ::iyr-simple string?)
(s/def ::eyr-simple string?)
(s/def ::hgt-simple string?)
(s/def ::hcl-simple string?)
(s/def ::ecl-simple string?)
(s/def ::pid-simple string?)
(s/def ::cid-simple string?)

(s/def ::simple (s/keys :req [::byr-simple
                              ::iyr-simple
                              ::eyr-simple
                              ::hgt-simple
                              ::hcl-simple
                              ::ecl-simple
                              ::pid-simple]
                        :opt [::cid-simple]))

(defn parse-long
  [v]
  (try
    (Long/parseLong v)
    (catch RuntimeException _
      nil)))

(s/def ::str->long (s/conformer #(or (parse-long %) ::s/invalid)
                                str))

(s/def ::str->inches (s/conformer (fn [hgt]
                                    (let [[_ in] (re-find #"(\d+)in" hgt)]
                                      (or (parse-long in)
                                          ::s/invalid)))))

(s/def ::str->centimeters (s/conformer (fn [hgt]
                                         (let [[_ cm] (re-find #"(\d+)cm" hgt)]
                                           (or (parse-long cm)
                                               ::s/invalid)))))
(s/def ::byr-strict (s/and ::byr-simple
                           ::str->long
                           #(<= 1920 % 2002)))

(s/def ::iyr-strict (s/and ::iyr-simple
                           ::str->long
                           #(<= 2010 % 2020)))

(s/def ::eyr-strict (s/and ::eyr-simple
                           ::str->long
                           #(<= 2020 % 2030)))

(s/def ::hgt-strict (s/or :centimeters (s/and ::hgt-simple
                                              ::str->centimeters
                                              #(<= 150 % 193))
                          :inches (s/and ::hgt-simple
                                         ::str->inches
                                         #(<= 59 % 76))))

(s/def ::hcl-strict (s/and ::hcl-simple
                           #(some? (re-matches #"#[a-f0-9]{6}" %))))

(s/def ::ecl-strict (s/and ::ecl-simple
                           #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}))

(s/def ::pid-strict (s/and ::pid-simple
                           #(some? (re-matches #"[0-9]{9}" %))))

(s/def ::cid-strict ::cid-simple)

(s/def ::strict (s/keys :req [::byr-strict
                              ::iyr-strict
                              ::eyr-strict
                              ::hgt-strict
                              ::hcl-strict
                              ::ecl-strict
                              ::pid-strict]
                        :opt [::cid-strict]))

(defn commit-pending
  [{:keys [pending] :as m}]
  (-> m
      (update :passports conj pending)
      (assoc :pending {})))

(def current-ns (str *ns*))

(defn collect-field
  [{:keys [prefix] :as m} field]
  (let [[_ k v] (re-find #"(\p{Graph}+):(\p{Graph}+)" field)
        key (keyword current-ns (str k "-" prefix))]
    (assoc-in m [:pending key] v)))

(defn process-field
  [m field]
  (if (empty? field)
    (commit-pending m)
    (collect-field m field)))

(defn- input->passports
  [input prefix]
  (->> (str/split input #"\s")
       (reduce
         process-field
         {:passports []
          :prefix    prefix
          :pending   {}})
       (commit-pending)
       (:passports)))

(defn validate
  [spec passport]
  (let [conformed (s/conform spec passport)]
    (if (s/invalid? conformed)
      nil
      conformed)))

(defn extract-passports
  [data spec]
  (->> (input->passports data (name spec))
       (map #(validate spec %))
       (filter some?)
       (into [])))

(defn count-valid
  [data spec]
  (->> (extract-passports data spec)
       (count)))

(comment
  (let [spec ::simple]
    [(count-valid sample-data spec)
     (count-valid (slurp (io/resource "day4.txt")) spec)])
  (let [spec ::strict]
    [(count-valid sample-data spec)
     (count-valid (slurp (io/resource "day4.txt")) spec)]))
