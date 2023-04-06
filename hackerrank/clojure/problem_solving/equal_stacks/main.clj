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
    (map #(map (fn [x] (Integer. x)) %))
    vector))

(defn equal-stacks
  [h1 h2 h3]
  (->>
   [h1 h2 h3]
   (map 
     #(->> %
        reverse
        (reductions +)))
   str))

(comment
  (let [[_header h1 h2 h3]
        (->> "input.txt"
             (slurp)
             (#(str/split % #"\n")))
        ;; [l1 l2 l3] (str/split header #" ")
        [h1 h2 h3] (map (fn [h] (map #(Integer. %) (str/split h #" "))) [h1 h2 h3])
        [h1 h2 h3] (map (comp (partial reductions +) reverse) [h1 h2 h3])
        max-val (first (apply intersection (map set [h1 h2 h3])))
        [h1 h2 h3] (map (partial drop-while #(<= % max-val)) [h1 h2 h3])
        val (reduce + (map count [h1 h2 h3]))]
    (println "max-val: " max-val)
    (println "h1: " h1)
    (println "h2: " h2)
    (println "h3: " h3)
    (println "val: " val)
    )
  
  )

(defn main []
  (let [filename "./input.txt"
        [h1 h2 h3] (read-input filename)
        result (equal-stacks h1 h2 h3)]
    (println result)))

(main)