(ns all-test
  (:require [clojure.test :as t]
            [day01]
            [day02]))

(t/deftest day1
  (let [d [1721 979 366 299 675 1456]]
    (t/is (= 514579 (day01/part1 d)))
    (t/is (= 241861950 (day01/part2 d)))))

(t/deftest day2
  (let [d [{:lbound 1 :ubound 3 :key \a :pw "abcde"}
           {:lbound 1 :ubound 3 :key \b :pw "cdefg"}
           {:lbound 2 :ubound 9 :key \c :pw "ccccccccc"}]]
    (t/is (= 2 (day02/part1 d)))
    (t/is (= 1 (day02/part2 d)))))
