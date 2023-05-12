(require '[clojure.string :as str])

(def tests 
  [["Hello World", 5]
   ["   fly me   to   the moon  ", 4]
   ["luffy is still joyboy", 6]])

(defn length-of-last-word [s]
  (-> s
      str/trim
      (str/split #" +")
      last
      count))

(defn -main [& _args]
  (println "Running...")
  (doall
   (for [[s o] tests]
     (let [res (length-of-last-word s)]
       (if (= res o)
         (println "Success")
         (println "Failure: " res " != " o))))))

(-main)
