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
  (let [last_index (dec (count input))]
    (loop
     [code-vec       input
      accumulator    0
      curr-idx       0
      seen-idx       #{}]
      (if (> curr-idx last_index)
        {:acc accumulator, :success true}
        (if (contains? seen-idx curr-idx)
          {:acc accumulator, :success false}
          ;; And here we need to update that we visited, and then update curr-idx
          (case ((code-vec curr-idx) :instr)
            ;; The acc command
            :acc (recur
                  code-vec
                  (+' accumulator (get-in code-vec [curr-idx :arg]))
                  (inc curr-idx)
                  (conj seen-idx curr-idx))

            ;; The jmp command
            :jmp (recur
                  code-vec
                  accumulator
                  (+' curr-idx (get-in code-vec [curr-idx :arg]))
                  (conj seen-idx curr-idx))

            ;; The nop command
            :nop (recur
                  code-vec
                  accumulator
                  (inc curr-idx)
                  (conj seen-idx curr-idx))))))))

(defn part1
  [code-vec]
  (:acc (main-loop code-vec)))

(defn nop-or-jmp?
  [input]
  (contains? #{:jmp :nop} (:instr (last input))))

(defn flip-code
  [code-vec idx]
  (update-in
   code-vec
   [idx :instr]
   #(case %
      :jmp :nop
      :nop :jmp)))

(defn part2
  [code-vec]
  (let [nops-and-jmps (filter nop-or-jmp? (map-indexed vector code-vec))
        idcs (map first nops-and-jmps)]
    (->> idcs
         (map #(main-loop (flip-code code-vec %)))
         (filter :success)
         first
         :acc)))

(defn run [opts]
  (let [code-vec (parse-input-str (slurp "./inputs/day08.txt"))]
    (println "day 08 part 1: " (part1 code-vec))
    (println "day 08 part 2: " (part2 code-vec))))
