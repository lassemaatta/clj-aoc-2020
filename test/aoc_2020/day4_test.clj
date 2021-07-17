(ns aoc-2020.day4-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.java.io :as io]
            [aoc-2020.day4 :as tasks]))

(deftest day4-tests
  (testing "with sample data"
    (let [data "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\nhcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm\n\nhcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in"]
      (testing "first task"
        (is (= 2
               (tasks/count-valid data ::tasks/simple))))
      (testing "second task"
        (is (= 2
               (tasks/count-valid data ::tasks/strict))))))

  (testing "with larger data"
    (let [data (slurp (io/resource "day4.txt"))]
      (testing "first task"
        (is (= 196
               (tasks/count-valid data ::tasks/simple))))
      (testing "second task"
        (is (= 114
               (tasks/count-valid data ::tasks/strict)))))))
