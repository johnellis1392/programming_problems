
(def tests
  [[[1 2 3] [1 2 4]]
   [[4 3 2 1] [4 3 2 2]]
   [[9] [1 0]]])

(defn plus-one
  ([v] (->> v reverse (plus-one 1) reverse))
  ([c v]
   (cond
     (= c 0) v
     (empty? v) [1]
     (> 9 (first v)) (cons (+ 1 (first v)) (rest v))
     :else (cons 0 (plus-one 1 (rest v))))))

(defn -main [& _args]
  (println "Running...")
  (doall
   (for [[i o] tests]
     (let [r (plus-one i)]
       (if (= r o)
         (println "Success")
         (println "Failure: " r " != " o))))))

(-main)
