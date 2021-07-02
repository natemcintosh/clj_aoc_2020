(ns day21
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-line
  "Input looks like
   mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
   and this function produces
   {:ingredients #{\"mxmxvkd\" \"kfcds\" \"sqjhc\" \"nhms\"}
   :contains #{\"dairy\" \"fish\"}}"
  [line]
  (let [[ingredient-str contains-str] (-> line
                                          (str/replace "(" "")
                                          (str/replace ")" "")
                                          (str/replace "," "")
                                          (str/split #"contains "))
        ingredients (set (str/split ingredient-str #"\s+"))
        contains    (set (str/split contains-str   #"\s+"))]
    {:ingredients ingredients, :contains contains}))


(defn parse-input-str
  [input-str]
  (mapv parse-line (str/split-lines input-str)))


(defn get-ingredient-for-allergen
  [ingredients]
  (apply set/intersection ingredients))

(defn get-rows-for-allergen
  [input allergen]
  (filter #(get-in % [:contains allergen]) input))

(defn get-ingredients-for-all-allergens
  [input]
  (let [allergens (apply set/union (map :contains input))
        rows-per-allergen (mapv
                           #(get-rows-for-allergen input %)
                           allergens)
        possible-ingredients-per-allergen (map #(map :ingredients %) rows-per-allergen)
        ingredient-allergen-map (zipmap 
                                 allergens 
                                 (map get-ingredient-for-allergen possible-ingredients-per-allergen))]
    ingredient-allergen-map))