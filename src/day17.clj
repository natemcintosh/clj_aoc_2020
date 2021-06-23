(ns day17
  (:require [clojure.string :as str]
            [clojure.set    :as set]
            [clojure.math.combinatorics :as combo]))

(defn find-all
  [s value]
  (loop [start-ind 0
         res       []]
    (let [found-ind (.indexOf s value start-ind)]
      (if (< found-ind 0)
        res
        (recur
         (inc found-ind)
         (conj res found-ind))))))

(defn pair-up
  "assumes data looks like [0 [1 2 3 4]] and produces ([0 1][0 2][0 3][0 4])"
  [[x coll]]
  (map #(vector x %) coll))

(defn parse-input-str
  [input-str]
  (let [xs (map #(find-all % "#") (str/split-lines input-str))
        y-with-xs (map-indexed vector xs)]
    (mapcat pair-up y-with-xs)))

(defn extend-dimensions-by-n
  "Takes a 2D coordinate and extends it
   with further n dimensions, filling the 
   rest with 0"
  [n coord]
  (into coord (repeat n 0)))

(defn gen-directions
  "Create a set of directions of nearest neighbors in n-dimensions. 
   if n-dims is 2, then we get 
   #{(0 1)(0 -1)(1 0)(-1 0)
     (1 1)(1 -1)(-1 1)(-1 -1)}"
  [n-dims]
  (let [zero-vec (set (vector (vec (repeat n-dims 0))))]
    (set/difference
     (set (apply combo/cartesian-product (repeat n-dims [-1 0 1])))
     zero-vec)))

(defn bubble-out
  "buble-out will return all the points from the starting point
   given a seq of directions"
  [dirs pt]
  (set (mapv #(map +' pt %) dirs)))

(defn count-active-neighbors
  "count-active-neighbors will bubble out from pt in all dirs,
   and see how many of those points are active-pts"
  [active-pts dirs pt]
  (let [neighbors (bubble-out dirs pt)]
    (count (set/intersection active-pts neighbors))))

(defn next-generation
  "next-generation will return the next generation of active points
   given a set of active-pts and directions of neighbors"
  [active-pts dirs]
  (let [active-to-inactive (set
                            (filterv
                             #(let [n-active-neighbors (count-active-neighbors active-pts dirs %)]
                                (and (not= 2 n-active-neighbors) (not= 3 n-active-neighbors)))
                             active-pts))

        bubbled-out-neighbors (set (mapcat (partial bubble-out dirs) active-pts))

        inactive-neighbors-of-active (set/difference bubbled-out-neighbors active-pts)

        inactive-to-active (set (filterv
                                 #(= 3 (count-active-neighbors active-pts dirs %))
                                 inactive-neighbors-of-active))]
    (set/union (set/difference active-pts active-to-inactive) inactive-to-active)))

(defn solve
  "With some input-pts and dirs, solve for n-generations"
  [input-pts dirs n-generations]
  (loop [cntr  0
         pts   input-pts]
    (if (= n-generations cntr)
      pts
      (recur
       (inc cntr)
       (next-generation pts dirs)))))

(defn part1
  [initial-2d-coords]
  (let [points-3d (set (map #(day17/extend-dimensions-by-n 1 %) initial-2d-coords))]
    (count (solve points-3d (gen-directions 3) 6))))

(defn part2
  [initial-2d-coords]
  (let [points-4d (set (map #(day17/extend-dimensions-by-n 2 %) initial-2d-coords))]
    (count (solve points-4d (gen-directions 4) 6))))

(defn run [opts]
  (let [initial-2d-coords (parse-input-str (slurp "./inputs/day17.txt"))]
    (println "day 17 part 1: " (part1 initial-2d-coords))
    (println "day 17 part 2: " (part2 initial-2d-coords))))
