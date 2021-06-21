(ns day08
  (:require [clojure.string :as str]))

(defn create-day08-rule
  [line]
  (let [m (zipmap [:instr :arg] (str/split line #"\s+"))]
    (-> m
        (update :instr keyword)
        (update :arg   #(Integer/parseInt %)))))

(defn parse-input-str
  [input-str]
  (mapv create-day08-rule (str/split-lines input-str)))

(defn main-loop
  [input]
  (loop
   [code-vec       input
    accumulator    0
    curr-idx       0
    seen-idx ^ints #{}]
    (if (contains? seen-idx curr-idx)
      accumulator
      ;; And here we need to update that we visited, and then update curr-idx
      (case ((code-vec curr-idx) :instr)
        ;; The acc command
        :acc (recur
              code-vec
              (+' accumulator ((code-vec curr-idx) :arg))
              (inc curr-idx)
              (conj seen-idx curr-idx))

        ;; The jmp command
        :jmp (recur
              code-vec
              accumulator
              (+' curr-idx ((code-vec curr-idx) :arg))
              (conj seen-idx curr-idx))

        ;; The nop command
        :nop (recur
              code-vec
              accumulator
              (inc curr-idx)
              (conj seen-idx curr-idx))))))

(defn part1
  [code-vec]
  (main-loop code-vec))

(defn run [opts]
  (let [code-vec (parse-input-str (slurp "./inputs/day08.txt"))]
    (println "day 08 part 1: " (part1 code-vec))))
