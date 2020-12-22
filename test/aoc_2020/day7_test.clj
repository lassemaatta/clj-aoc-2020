(ns aoc-2020.day7-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc-2020.day7 :as tasks]))

(deftest day7-tests
  (testing "with sample data"
    (let [data "light red bags contain 1 bright white bag, 2 muted yellow bags.\n\ndark orange bags contain 3 bright white bags, 4 muted yellow bags.\n\nbright white bags contain 1 shiny gold bag.\n\nmuted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\n\nshiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\n\ndark olive bags contain 3 faded blue bags, 4 dotted black bags.\n\nvibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\n\nfaded blue bags contain no other bags.\n\ndotted black bags contain no other bags."]
      (testing "first task"
        (is (= 4
               (tasks/count-parents data "shiny gold"))))
      (testing "second task"
        (is (= 32
               (tasks/count-children data "shiny gold"))))))

  (testing "with larger data"
    (let [data (slurp (io/resource "day7.txt"))]
      (testing "first task"
        (is (= 224
               (tasks/count-parents data "shiny gold"))))
      (testing "second task"
        (is (= 1488
               (tasks/count-children data "shiny gold")))))))
