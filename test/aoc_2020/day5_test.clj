(ns aoc-2020.day5-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.java.io :as io]
            [aoc-2020.day5 :as tasks]))

(deftest day5-tests
  (testing "with sample data"
    (let [data "BFFFBBFRRR\nFFFBBBFRRR\nBBFFBBFRLL"]
      (testing "first task"
        (is (= 820
               (tasks/max-seat data))))
      (testing "second task"
        (is (= 820
               (tasks/find-seat data))))))

  (testing "with larger data"
    (let [data (slurp (io/resource "day5.txt"))]
      (testing "first task"
        (is (= 963
               (tasks/max-seat data))))
      (testing "second task"
        (is (= 592
               (tasks/find-seat data)))))))
