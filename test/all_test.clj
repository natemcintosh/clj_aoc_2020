(ns all-test
  (:require [clojure.test :as t]
            [day01]
            [day02]
            [day04]))

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

(t/deftest day4
  (let [d [{:ecl "gry"
            :pid 860033327
            :eyr 2020
            :hcl "#fffffd"
            :byr 1937
            :iyr 2017
            :cid 147
            :hgt "183cm"}
           {:iyr 2013
            :ecl "amb"
            :cid 350
            :eyr 2023
            :pid 28048884
            :hcl "#cfa07d"
            :byr 1929}
           {:hcl "#ae17e1"
            :iyr 2013
            :eyr 2024
            :ecl "brn"
            :pid 760753108
            :byr 1931
            :hgt "179cm"}
           {:hcl "#cfa07d"
            :eyr 2025
            :pid 166559648
            :iyr 2011
            :ecl "brn"
            :hgt "59in"}]
        b-valid       {:byr 2002}
        b-invalid     {:byr 2003}
        hgt-valid-1   {:hgt "60in"}
        hgt-valid-2   {:hgt "190cm"}
        hgt-invalid-1 {:hgt "190in"}
        hgt-invalid-2 {:hgt "60"}]
    (t/is (= 2 (day04/part1 d)))
    (t/is (= true (day04/byr? b-valid)))
    (t/is (= false (day04/byr? b-invalid)))
    (t/is (= true (day04/hgt? hgt-valid-1)))
    (t/is (= true (day04/hgt? hgt-valid-2)))
    (t/is (= false (day04/hgt? hgt-invalid-1)))
    (t/is (= false (day04/hgt? hgt-invalid-2)))))








