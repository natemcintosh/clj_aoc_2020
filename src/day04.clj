(ns day04
  (:require [clojure.string :as str]))


(defn try-parse-int
  [i]
  (try (Integer/parseUnsignedInt i)
       (catch Exception _ (identity i))))

(defn parse-passport-str
  [line]
  (let [kv-strs (-> line
                    (str/replace "\n" " ")
                    (str/split #"\s+"))
        ps      (map #(str/split % #":") kv-strs)
        ks      (map (comp keyword first) ps)
        vs      (map (comp try-parse-int second) ps)]
    (zipmap ks vs)))

(defn parse-input-file
  [fname]
  (mapv
   parse-passport-str
   (str/split (slurp fname) #"\n\n")))

(defn part1
  [input]
  (count
   (filter
    (every-pred :byr :iyr :eyr :hgt :hcl :ecl :pid)
    input)))

(defn digits [n]
  (->> n
       (iterate #(quot % 10))
       (take-while pos?)
       (mapv #(mod % 10))
       rseq))

(defn byr?
  [input]
  (let [y (:byr input)]
    (and
     (>= y 1920)
     (<= y 2002)
     (= 4 (count (digits y))))))

(defn iyr?
  [input]
  (let [y (:iyr input)]
    (and
     (>= y 2010)
     (<= y 2020)
     (= 4 (count (digits y))))))

(defn eyr?
  [input]
  (let [y (:eyr input)]
    (and
     (>= y 2020)
     (<= y 2030)
     (= 4 (count (digits y))))))

(defn hgt?
  [input]
  (let [h (:hgt input)
        last-char (last h)
        n (Integer/parseUnsignedInt
           (apply str (reverse (nthrest (reverse h) 2))))]
    (or
     (and (= \m last-char) (>= n 150) (<= n 2030))
     (and (= \n last-char) (>= n 59)  (<= n 76))
     false)))

(defn run [opts]
  (let [input (parse-input-file "/Users/mcintna1/dev/clj_aoc_2020/inputs/day04.txt")]
    (println "day 04 part 1: " (part1 input))))