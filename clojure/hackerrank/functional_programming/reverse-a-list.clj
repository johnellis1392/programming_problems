#_(require '[clojure.string :as str])

#_(defn reverse-a-list [list]
  (if (empty? list) []
      (conj (reverse-a-list (rest list)) (first list))))

#_(def test-1
  "19
22
3
28
26
17
18
4
28
0
")

#_(defn parse-input [input]
  (str/split input #"\s+"))

(comment
  (-> test-1
      (str/split #"\s+")
      (#(map int %))
      (reverse-a-list))

  (conj [1 2 3] 4))

#_(defn main []
  (let [input (parse-input (slurp *in*))]
    (reverse-a-list input)))

(defn reverse-list [acc lst]
    (if (empty? lst) acc
        (reverse-list (conj acc (first lst)) (rest lst))))

(->>
    (line-seq (clojure.java.io/reader *in*))
    (filter #(not (empty? %)))
    (map #(Integer/parseInt %))
    (reverse-list [])
    (clojure.string/join "\n")
    print)

