(require 
 '[clojure.string :as str]
 '[clojure.set :refer [intersection]])

(defn zip [& vs]
  (apply (partial map vector) vs))

(defn read-input [filename]
  (->> 
   (slurp filename)
   (#(str/split % #"\n"))
   (map #(str/split % #" "))
   (rest)
   (map #(map (fn [x] (Integer. x)) %))))

(defn equal-stacks
  [stacks]
  (->>
   stacks
   (map (comp (partial reductions +) reverse))
   (map set)
   (apply intersection)
   (#(if (empty? %) 0 (apply max %)))))

(defn main []
  (let [filename "./input.2.txt"]
    (->>
     filename
     read-input
     equal-stacks
     (println))))

(main)