(ns day03
  (:require [clojure.string :as str]
            [criterium.core :refer [quick-bench]]))


(defn try-parse-int
  [i]
  (try (Integer/parseUnsignedInt i)
       (catch Exception e i)))

(defn parse-passport-str
  [input]
  (let [kv-strs (-> input
                    (str/replace "\n" " ")
                    (str/split #"\s+"))
        ps       (map #(str/split % #":") kv-strs)
        ks       (map (comp keyword first) ps)
        vs       (map (comp second) ps)]
    (prn kv-strs)
    (prn ps)
    (prn ks)
    (prn vs)))