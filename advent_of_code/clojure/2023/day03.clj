(ns day03
  (:require [clojure.string :as str]))

(def test-input
  "467..114..
   ...*......
   ..35..633.
   ......#...
   617*......
   .....+.58.
   ..592.....
   ......755.
   ...$.*....
   .664.598..")

(def adjacent-point-set
  [[-1 -1] [-1 0] [-1 1]
   [0 -1] [0 1]
   [1 -1] [1 0] [1 1]])

(defn read-input [input]
  (->>
   input
   str/trim
   str/split-lines
   (map str/trim)
   (map #(map str %))
   (map vec)
   vec))

(defn size [grid]
  [(count grid) (count (first grid))])

(defn valid-point [grid r c]
  (let [[rows cols] (size grid)]
    (and
     (>= r 0)
     (>= c 0)
     (< r rows)
     (< c cols))))

(defn add-points [[r1 c1] [r2 c2]]
  [(+ r1 r2) (+ c1 c2)])

(defn adjacents [grid r c]
  (->>
   adjacent-point-set
   (map (partial add-points [r c]))
   (filter (partial apply valid-point grid))))

(defn grid-get [grid r c]
  (if (valid-point grid r c)
    ((grid r) c)
    nil))

(let [grid (read-input test-input)]
 (grid-get grid 1 1))

(defn concat-map [f coll]
  (apply concat (map f coll)))

(defn is-digit? [s]
  #_(println s)
  (cond
    (nil? s) false
    (re-matches #"[0-9]+" s) true
    :else false))

(defn is-symbol? [s]
  (and (not (is-digit? s)) (not (= "." s))))

(defn zip [a b] (map vector a b))
(defn enumerate [coll] (zip (range 0 (count coll)) coll))

(defn grid-get-symbols [grid]
  (->>
   grid
   enumerate
   (concat-map
    (fn [[r row]]
      (->>
       (enumerate row)
       (map
        (fn [[c value]]
          (if (is-symbol? value) [r c] [])))
       (filter seq))))))

;; Get symbols adjacent to numbers
(comment
  (let [grid (read-input test-input)]
    (->>
     grid
     grid-get-symbols
     (filter (fn [[r c]]
               (->>
                (adjacents grid r c)
                (some (fn [[r2 c2]]
                        (is-digit? (grid-get grid r2 c2))))))))))

(defn split [pred coll]
  (cond 
    (empty? coll) []

    (pred (first coll)) 
    (cons (take-while pred coll) (split pred (drop-while pred coll)))
    
    :else
    (cons (take-while (comp not pred) coll) (split pred (drop-while (comp not pred) coll) ))))

(defn collect-contiguous [pred coll]
  (cond
    (empty? coll) []
    
    (pred (first coll))
    (cons (take-while pred coll) (collect-contiguous pred (drop-while pred coll)))
    
    :else
    (collect-contiguous pred (drop-while (comp not pred) coll))))

(let [grid (read-input test-input)]
  (->>
   grid
   (map enumerate)
   enumerate
   (map (fn [[r row]]
          (->>
           row
           (collect-contiguous (comp is-digit? second))
           (map (fn [n]
                  (map (fn [[c v]] [[r c] v]) n))))))
   (filter seq)))

(defn row-get-numbers [r row]
  (println ", count = " (count row) ", row = " row)
  (cond
    (empty? row) []
    
    (is-digit? (second (first row)))
    (let [numbers (take-while (comp is-digit? second) row)
          n (Integer/parseInt (reduce str "" (map second numbers)))
          points (map (fn [[c _]] [r c]) numbers)
          rest (drop-while (comp is-digit? second) row)]
      (cons [n points] (row-get-numbers r rest)))
    
    :else
    (let [rest (drop-while (comp not is-digit? second) row)]
      (row-get-numbers r rest))))

(defn grid-get-numbers [grid]
  (let [rows (enumerate grid)]
    (println rows)
    (map (fn [[r row]] 
           #_(println r row)
           (row-get-numbers r row)) rows)))

(let [input (->> (read-input test-input))
      #_#_input (enumerate input)
      #_#_numbers (row-get-numbers 0 (second (first input)))
      #_#_x numbers]
  (grid-get-numbers input))

(defn grid-get-numbers [grid]
  (->>
   grid
   enumerate
   (map
    (fn [[r row]]
      ))))

(let [grid (read-input test-input)]
  (->>
   grid
   enumerate
   (map
    (fn [[r row]] 
      row))))

(defn part1 [grid]
  0)

(defn part2 [grid]
  0)

(defn -main []
  (let [filename "input.txt"
        debug true
        input (if debug test-input (slurp filename))
        input (read-input input)]
    (println "2023 Day 3, Part 1: " (part1 input))
    (println "2023 Day 3, Part 2: " (part2 input))))

(-main)
