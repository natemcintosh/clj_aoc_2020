(ns day02
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.pprint :as pp]
            [clojure.spec.test.alpha :as stest]))


(defn count-in-coll
  "Count the number of times an item appears in a coll"
  [item coll]
  (count (filter #(= item %) (seq coll))))


(defn create-day2-rule
  "parse a single line from day 2 input into a map"
  [line]
  (let [values (-> line
                   (str/replace "-" " ")
                   (str/replace ":" "")
                   (str/split #" "))
        starting_map (zipmap [::lbound ::ubound ::key ::pw] values)]
    (-> starting_map
        (update ::lbound #(Integer/parseUnsignedInt %))
        (update ::ubound #(Integer/parseUnsignedInt %))
        (update ::key first))))


;; (s/def ::lbound int?)
;; (s/def ::ubound int?)
;; (s/def ::key    char?)
;; (s/def ::pw     string?)
;; (s/def ::day02-rule
;;   (s/keys
;;    :req [::lbound ::ubound ::key ::pw]))
;; (s/fdef create-day2-rule
;;   :args (s/cat ::line string?)
;;   :ret  ::day02-rule)


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
  [(= (:key m) (nth (:pw m) (- (:lbound m) 1)))
   (= (:key m) (nth (:pw m) (- (:ubound m) 1)))])


(defn part2
  [input]
  (->> input
       (filter #(= 1 (count (filter identity (count-key-matches-bounds %)))))
       count))

;; (defn part2
;;   [input]
;;   (count
;;    (filter
;;     #(= 1 (count (filter identity (count-key-matches-bounds %))))
;;     input)))


(defn run [opts]
  (let [input (parse-input "/Users/mcintna1/dev/aoc_2020/inputs/day02.txt")]
    (println "day 02 part 1: " (part1 input))
    (println "day 02 part 2: " (part2 input))))