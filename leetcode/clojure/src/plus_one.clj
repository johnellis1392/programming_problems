(ns plus-one)

(defn plus-one
  ([v] (->> v reverse (plus-one 1) reverse))
  ([c v]
   (cond
     (= c 0) v
     (empty? v) [1]
     (> 9 (first v)) (cons (+ 1 (first v)) (rest v))
     :else (cons 0 (plus-one 1 (rest v))))))
