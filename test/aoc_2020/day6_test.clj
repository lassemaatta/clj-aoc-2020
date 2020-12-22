(ns aoc-2020.day6-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc-2020.day6 :as tasks]))

(deftest day6-tests
  (testing "with sample data"
    (let [data "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb"]
      (testing "first task"
        (is (= 11
               (tasks/parse-and-count data :any))))
      (testing "second task"
        (is (= 6
               (tasks/parse-and-count data :all))))))

  (testing "with larger data"
    (let [data (slurp (io/resource "day6.txt"))]
      (testing "first task"
        (is (= 6763
               (tasks/parse-and-count data :any))))
      (testing "second task"
        (is (= 3512
               (tasks/parse-and-count data :all)))))))
