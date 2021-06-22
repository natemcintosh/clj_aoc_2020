(ns day17
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn find-all
  [s value]
  (loop [start-ind 0
         res       []]
    (let [found-ind (.indexOf s value start-ind)]
      (if (>= found-ind 0)
        (recur
         (inc found-ind)
         (conj res found-ind))
        res))))