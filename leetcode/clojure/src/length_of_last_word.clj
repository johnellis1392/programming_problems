(ns length-of-last-word
  (:require [clojure.string :as str]))

(defn length-of-last-word [s]
  (-> s
      str/trim
      (str/split #" +")
      last
      count))
