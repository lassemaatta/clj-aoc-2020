(ns aoc-2020.day9-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc-2020.day9 :as tasks]))

(deftest day9-tests
  (testing "with sample data"
    (let [data "35\n20\n15\n25\n47\n40\n62\n55\n65\n95\n102\n117\n150\n182\n127\n219\n299\n277\n309\n576"]
      (testing "first task"
        (is (= 127
               (tasks/problem-1 data 5))))
      (testing "second task"
        (is (= 62
               (tasks/problem-2 data 127))))))

  (testing "with larger data"
    (let [data (slurp (io/resource "day9.txt"))]
      (testing "first task"
        (is (= 1504371145
               (tasks/problem-1 data 25))))
      (testing "second task"
        (is (= 183278487
               (tasks/problem-2 data 1504371145)))))))
