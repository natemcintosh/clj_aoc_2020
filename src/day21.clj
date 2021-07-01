(ns day21
  (:require [clojure.string :as str]))

(defn parse-line
  "Input looks like
   mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
   and this function produces
   {:ingredients #{\"mxmxvkd\" \"kfcds\" \"sqjhc\" \"nhms\"}
  :contains #{\"dairy\" \"fish\"}}"
  [line]
  (let [[ingredients contains] (-> line
                                   (str/replace "(" "")
                                   (str/replace ")" "")
                                   (str/replace "," "")
                                   (str/split #"contains ")
                                   (map #(str/split % #"\s+")))]
    (prn ingredients)
    (prn contains)))