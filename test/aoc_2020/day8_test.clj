(ns aoc-2020.day8-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc-2020.day8 :as tasks]))

(deftest day8-tests
  (testing "with sample data"
    (let [data "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6"]
      (testing "first task"
        (is (= 5
               (:accum (tasks/run-until-loop data)))))
      (testing "second task"
        (is (= 8
               (:accum (tasks/run-traces data)))))))

  (testing "with larger data"
    (let [data (slurp (io/resource "day8.txt"))]
      (testing "first task"
        (is (= 1782
               (:accum (tasks/run-until-loop data)))))
      (testing "second task"
        (is (= 797
               (:accum (tasks/run-traces data))))))))
