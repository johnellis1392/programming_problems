(ns day02
  (:require [clojure.string :as str]))


(def test-input
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
   Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
   Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
   Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
   Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(defn parse-roll [roll]
  (let [[n color] (str/split roll #" ")]
    [(Integer/parseInt n) color]))

(defn parse-round [round]
  (map parse-roll (str/split round #", ")))

(defn parse-game [game]
  (let [[id rounds] (str/split game #": ")
        id (Integer/parseInt (str/join (drop (count "Game ") id)))
        rounds (str/split rounds #"; ")]
    [id (map parse-round rounds)]))


(defn read-input [input]
  (->>
   input
   (str/split-lines)
   (map str/trim)
   (map parse-game)))

(defn aggregate-rolls
  [[r1 g1 b1] [r2 g2 b2]]
  [(max r1 r2) (max g1 g2) (max b1 b2)])

(defn process-roll [[n color]]
  (case color
    "red" [n 0 0]
    "green" [0 n 0]
    "blue" [0 0 n]))

(defn valid-round? [[r g b]]
  (and
   (<= r 12)
   (<= g 13)
   (<= b 14)))

(defn process-round [rolls]
  (->>
   rolls
   (map process-roll)
   (reduce aggregate-rolls [0 0 0])))

(defn valid-game? [[_id rounds]]
  (->>
   rounds
   (map process-round)
   (every? valid-round?)))

(let [games (read-input test-input)]
  (->>
   games
   first
   valid-game?))

(defn part1 [games]
  (->>
   games
   (filter valid-game?)
   (map (fn [[id _]] id))
   (reduce + 0)))

(defn power-set [[_ rounds]]
  (->>
   rounds
   (map process-round)
   (reduce aggregate-rolls [0 0 0])
   (reduce * 1)))

(defn part2 [games] 
  (->>
   games
   (map power-set)
   (reduce + 0)))

(defn -main [& _args]
  (let [filename "input.txt"
        debug false
        input (if debug test-input (slurp filename))
        games (read-input input)]
    (println "2023 Day 2, Part 1: " (part1 games))
    (println "2023 Day 2, Part 2: " (part2 games))))

(-main)
