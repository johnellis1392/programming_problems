(defn f_v1
  ([list] (f_v1 list (count list)))
  ([list n] 
    ;; (println list)
    (cond
      (empty? list) nil
      (= 0 (mod n 2))
        (cons (first list) (f_v1 (rest list) (dec n)))
      :else
        (f_v1 (rest list) (dec n)))))

(defn f [list]
  (->> list 
    (map vector (range))
    (filter (comp #(= 0 (mod % 2)) first))
    (map second)))

(print (f [1 2 3 4]))