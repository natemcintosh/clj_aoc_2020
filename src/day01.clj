(ns day01
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

(defn parse-input
  "read the file and return vec of ints"
  [fname]
  (mapv #(Integer/parseUnsignedInt %) (str/split-lines (slurp fname))))

(defn part1
  [input]
  (let [nums (filter #(= 2020 (apply +' %)) (combo/combinations input 2))]
    (reduce *' (nth nums 0))))

(defn part2
  [input]
  (let [nums (filter #(= 2020 (apply +' %)) (combo/combinations input 3))]
    (reduce *' (nth nums 0))))

(defn run [opts]
  (let [input (parse-input "./inputs/day01.txt")]
    (println "day 01 part 1: " (part1 input))
    (println "day 01 part 2: " (part2 input))))
