(ns length-of-last-word-test
  (:require [clojure.test :refer [deftest testing is]]
            [length-of-last-word :refer [length-of-last-word]]))

(deftest length-of-last-word-test
  (testing "basic"
    (is (= (length-of-last-word "Hello World") 5))
    (is (= (length-of-last-word "   fly me   to   the moon  ") 4))
    (is (= (length-of-last-word "luffy is still joyboy") 6))))
