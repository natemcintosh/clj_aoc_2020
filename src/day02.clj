(ns day02
  (:require [clojure.string :as str]))


(defn count-in-coll
  "Count the number of times an item appears in a coll"
  [item coll]
  (count (filter #(= item %) (seq coll))))


(defn create-day2-rule
  "parse a single line from day 2 input into a map"
  [line]
  (let [values       (-> line
                         (str/replace "-" " ")
                         (str/replace ":" "")
                         (str/split #" "))
        starting_map (zipmap [:lbound :ubound :key :pw] values)]

    (-> starting_map
        (update :lbound #(Integer/parseUnsignedInt %))
        (update :ubound #(Integer/parseUnsignedInt %))
        (update :key    first))))


(defn parse-input
  "parse the input file"
  [fname]
  (mapv create-day2-rule (str/split-lines (slurp fname))))


(defn part1
  [input]
  (->> input
       (filter #(let [n (count-in-coll (:key %) (:pw %))]
                  (and
                   (<= (:lbound %) n)
                   (>= (:ubound %) n))))
       count))


(defn count-key-matches-bounds
  [m]
  (count
   (filter identity
           [(= (:key m) (nth (:pw m) (- (:lbound m) 1)))
            (= (:key m) (nth (:pw m) (- (:ubound m) 1)))])))


(defn part2
  [input]
  (count
   (filter
    #(= 1 (count-key-matches-bounds %))
    input)))


(defn run [opts]
  (let [input (parse-input "/Users/mcintna1/dev/aoc_2020/inputs/day02.txt")]
    (println "day 02 part 1: " (part1 input))
    (println "day 02 part 2: " (part2 input))))