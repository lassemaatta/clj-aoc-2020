(ns aoc-2020.day1-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc-2020.day1 :as tasks]))

(deftest day1-tests
  (testing "with sample data"
    (let [data "1721\n979\n366\n299\n675\n1456"]
      (testing "first task"
        (is (= 514579
               (tasks/find-two-entries data))))
      (testing "second task"
        (is (= 241861950
               (tasks/find-three-entries data))))))

  (testing "with larger data"
    (let [data (slurp (io/resource "day1.txt"))]
      (testing "first task"
        (is (= 692916
               (tasks/find-two-entries data))))
      (testing "second task"
        (is (= 289270976
               (tasks/find-three-entries data)))))))
