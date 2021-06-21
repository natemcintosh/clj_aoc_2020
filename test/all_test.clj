(ns all-test
  (:require [clojure.test :as t]
            [day01]
            [day02]
            [day04]
            [day06]
            [day08]))

(t/deftest day01
  (let [d [1721 979 366 299 675 1456]]
    (t/is (= 514579 (day01/part1 d)))
    (t/is (= 241861950 (day01/part2 d)))))

(t/deftest day02
  (let [d [{:lbound 1 :ubound 3 :key \a :pw "abcde"}
           {:lbound 1 :ubound 3 :key \b :pw "cdefg"}
           {:lbound 2 :ubound 9 :key \c :pw "ccccccccc"}]]
    (t/is (= 2 (day02/part1 d)))
    (t/is (= 1 (day02/part2 d)))))

(t/deftest day04-part1
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
            :hgt "59in"}]]
    (t/is (= 2      (day04/part1 d)))))

(t/deftest day04-part2
  (let [b-valid       {:byr 2002}
        b-invalid     {:byr 2003}
        hgt-valid-1   {:hgt "60in"}
        hgt-valid-2   {:hgt "190cm"}
        hgt-invalid-1 {:hgt "190in"}
        hgt-invalid-2 {:hgt "60"}
        hcl-valid-1   {:hcl "#123abc"}
        hcl-invalid-1 {:hcl "#123abz"}
        hcl-invalid-2 {:hcl "123abc"}
        ecl-valid     {:ecl "brn"}
        ecl-invalid   {:ecl "wat"}
        pid-valid     {:pid "000000001"}
        pid-invalid   {:pid "0123456789"}
        invalid-p1 "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"
        invalid-p2 "iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946"
        invalid-p3 "hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277"
        invalid-p4 "hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007"
        valid-p1 "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f"
        valid-p2 "eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"
        valid-p3 "hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022"
        valid-p4 "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"]
    (t/is (= true   (day04/byr? b-valid)))
    (t/is (= false  (day04/byr? b-invalid)))
    (t/is (= true   (day04/hgt? hgt-valid-1)))
    (t/is (= true   (day04/hgt? hgt-valid-2)))
    (t/is (= false  (day04/hgt? hgt-invalid-1)))
    (t/is (= false  (day04/hgt? hgt-invalid-2)))
    (t/is (= true   (day04/hcl? hcl-valid-1)))
    (t/is (= false  (day04/hcl? hcl-invalid-1)))
    (t/is (= false  (day04/hcl? hcl-invalid-2)))
    (t/is (= true   (day04/ecl? ecl-valid)))
    (t/is (= false  (day04/ecl? ecl-invalid)))
    (t/is (= true   (day04/pid? pid-valid)))
    (t/is (= false  (day04/pid? pid-invalid)))
    (t/is (= false  (day04/passes-part-2? (day04/parse-passport-str invalid-p1))))
    (t/is (= false  (day04/passes-part-2? (day04/parse-passport-str invalid-p2))))
    (t/is (= false  (day04/passes-part-2? (day04/parse-passport-str invalid-p3))))
    (t/is (= false  (day04/passes-part-2? (day04/parse-passport-str invalid-p4))))
    (t/is (= true   (day04/passes-part-2? (day04/parse-passport-str valid-p1))))
    (t/is (= true   (day04/passes-part-2? (day04/parse-passport-str valid-p2))))
    (t/is (= true   (day04/passes-part-2? (day04/parse-passport-str valid-p3))))
    (t/is (= true   (day04/passes-part-2? (day04/parse-passport-str valid-p4))))))

(t/deftest day06
  (let [d "abc

a
b
c

ab
ac

a
a
a
a

b"]
    (t/is (= 11 (day06/part1 (day06/get-sets-of-answers d))))
    (t/is (= 6  (day06/part2 d)))))

(t/deftest day08
  (let [input-str "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"
        parsed-input [{:instr :nop :arg 0}
                      {:instr :acc :arg 1}
                      {:instr :jmp :arg 4}
                      {:instr :acc :arg 3}
                      {:instr :jmp :arg -3}
                      {:instr :acc :arg -99}
                      {:instr :acc :arg 1}
                      {:instr :jmp :arg -4}
                      {:instr :acc :arg 6}]]
    (t/testing "Correct parsing of input string"
               (t/is (= parsed-input (day08/parse-input-str input-str))))
    
    (t/testing "Part 1 is correct"
               (t/is (= 5 (day08/part1 parsed-input))))))
