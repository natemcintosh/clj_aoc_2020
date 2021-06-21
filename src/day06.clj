(ns day06
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn get-sets-of-answers
  [input-str]
  (let [chunks   (str/split input-str #"\n\n")
        raw-strs (map #(str/replace % "\n" "") chunks)]
    (mapv #(into #{} %) raw-strs)))

(defn part1
  [answers]
  (reduce +' (map count answers)))

(defn part2
  [input-str]
  (let [group-chunks      (str/split input-str #"\n\n")
        individual-chunks (map #(str/split-lines %) group-chunks)
        ind-sets          (map #(map set %) individual-chunks)
        yes-for-everyone  (map #(apply set/intersection %) ind-sets)]
    (reduce +' (map count yes-for-everyone))))

(defn run [opts]
  (let [input-str (slurp "./inputs/day06.txt")
        answers (get-sets-of-answers input-str)]
    (println "day 06 part 1: " (part1 answers))
    (println "day 06 part 2: " (part2 input-str))))