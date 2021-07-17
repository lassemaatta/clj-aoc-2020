(ns aoc-2020.day10-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.java.io :as io]
            [aoc-2020.day10 :as tasks]))

(deftest day10-tests
  (testing "with sample data"
    (let [data "16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4"]
      (testing "first task"
        (is (= 35
               (tasks/task-1 data))))
      (testing "second task"
        (is (= 8
               (tasks/calculate-paths data))))))

  (testing "with another sample data"
    (let [data "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3"]
      (testing "first task"
        (is (= 220
               (tasks/task-1 data))))
      (testing "second task"
        (is (= 19208
               (tasks/calculate-paths data))))))

  (testing "with larger data"
    (let [data (slurp (io/resource "day10.txt"))]
      (testing "first task"
        (is (= 1885
               (tasks/task-1 data))))
      (testing "second task"
        (is (= 2024782584832
               (tasks/calculate-paths data)))))))
