(ns aoc-2020.day3-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.java.io :as io]
            [aoc-2020.day3 :as tasks]))

(deftest day3-tests
  (testing "with sample data"
    (let [data "..##.......\n#...#...#..\n.#....#..#.\n..#.#...#.#\n.#...##..#.\n..#.##.....\n.#.#.#....#\n.#........#\n#.##...#...\n#...##....#\n.#..#...#.#"]
      (testing "first task"
        (is (= 7
               (tasks/problem-1 data))))
      (testing "second task"
        (is (= 336
               (tasks/problem-2 data))))))

  (testing "with larger data"
    (let [data (slurp (io/resource "day3.txt"))]
      (testing "first task"
        (is (= 198
               (tasks/problem-1 data))))
      (testing "second task"
        (is (= 5140884672
               (tasks/problem-2 data)))))))
