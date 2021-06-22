(ns day04
  (:require [clojure.string :as str]))


(defn try-parse-int
  [i]
  (try (Integer/parseUnsignedInt i)
       (catch Exception _ i)))

(defn parse-pairs
  [kv-str]
  (let [k (first kv-str)
        v (last kv-str)]
    (if (contains? #{"pid" "hgt" "hcl"} k)
      v
      (try-parse-int v))))

(defn parse-passport-str
  [line]
  (let [kv-strs (-> line
                    (str/replace "\n" " ")
                    (str/split #"\s+"))
        ps      (map #(str/split % #":") kv-strs)
        ks      (map (comp keyword first) ps)
        vs      (map parse-pairs ps)]
    (zipmap ks vs)))

(defn parse-input-file-part-1
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
  [{y :byr}]
  (and
   (>= y 1920)
   (<= y 2002)
   (= 4 (count (digits y)))))

(defn iyr?
  [{y :iyr}]
  (and
   (>= y 2010)
   (<= y 2020)
   (= 4 (count (digits y)))))

(defn eyr?
  [{y :eyr}]
  (and
   (>= y 2020)
   (<= y 2030)
   (= 4 (count (digits y)))))

(defn hgt?
  [input]
  (let [h (:hgt input)
        last-char (last h)
        num-chars #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9}
        n (Integer/parseUnsignedInt
           (apply str (take-while #(contains? num-chars %) h)))]
    (or
     (and (= \m last-char) (>= n 150) (<= n 2030))
     (and (= \n last-char) (>= n 59)  (<= n 76)))))


(defn hcl?
  [input]
  (let [h (:hcl input)
        hair-chars (rest h)
        valid-chars #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \a \b \c \d \e \f}]
    (and
     (= \# (first h))
     (every? #(contains? valid-chars %) hair-chars))))


(defn ecl?
  [input]
  (contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} (:ecl input)))

(defn pid?
  [input]
  (let [p (:pid input)
        num-chars #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9}]
    (and
     (= 9 (count p))
     (every? #(contains? num-chars %) p))))


(defn passes-part-2?
  [input]
  (let [has-all-keys ((every-pred :byr :iyr :eyr :hgt :hcl :ecl :pid) input)]
    (if has-all-keys
      ((every-pred byr? iyr? eyr? hgt? hcl? ecl? pid?) input)
      false)))


(defn part2
  [input-str]
  (let [passport-strs (str/split input-str #"\n\n")
        passports (mapv parse-passport-str passport-strs)]
    (count
     (filter
      passes-part-2?
      passports))))


(defn run [opts]
  (let [input-1 (parse-input-file-part-1 "./inputs/day04.txt")
        input-2 (slurp "./inputs/day04.txt")]
    (println "day 04 part 1: " (part1 input-1))
    (println "day 04 part 2: " (part2 input-2))))