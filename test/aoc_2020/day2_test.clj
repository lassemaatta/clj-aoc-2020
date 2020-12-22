(ns aoc-2020.day2-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc-2020.day2 :as tasks]))

(deftest day2-tests
  (testing "with sample data"
    (let [data "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc"]
      (testing "first task"
        (is (= 2
               (tasks/count-valid data ::tasks/valid-policy-1-row))))
      (testing "second task"
        (is (= 1
               (tasks/count-valid data ::tasks/valid-policy-2-row))))))

  (testing "with larger data"
    (let [data (slurp (io/resource "day2.txt"))]
      (testing "first task"
        (is (= 528
               (tasks/count-valid data ::tasks/valid-policy-1-row))))
      (testing "second task"
        (is (= 497
               (tasks/count-valid data ::tasks/valid-policy-2-row)))))))
