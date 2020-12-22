(ns aoc-2020.day12-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc-2020.day12 :as tasks]))

(deftest day12-tests
  (testing "with sample data"
    (let [data "F10\nN3\nF7\nR90\nF11"]
      (testing "first task"
        (is (= 25
               (tasks/data->distance data))))
      (testing "second task"
        (is (= 286
               (tasks/data->waypoint-distance data))))))

  (testing "with larger data"
    (let [data (slurp (io/resource "day12.txt"))]
      (testing "first task"
        (is (= 759
               (tasks/data->distance data))))
      (testing "second task"
        (is (= 45763
               (tasks/data->waypoint-distance data)))))))
