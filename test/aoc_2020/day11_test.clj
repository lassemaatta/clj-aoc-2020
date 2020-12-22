(ns aoc-2020.day11-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc-2020.day11 :as tasks]))

(deftest day11-tests
  (testing "with sample data"
    (let [data "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL"]
      (testing "first task"
        (is (= 37
               (tasks/task-1 data))))
      (testing "second task"
        (is (= 26
               (tasks/task-2 data))))))

  (testing "with larger data"
    (let [data (slurp (io/resource "day11.txt"))]
      (testing "first task"
        (is (= 2183
               (tasks/task-1 data))))
      (testing "second task"
        (is (= 1990
               (tasks/task-2 data)))))))
