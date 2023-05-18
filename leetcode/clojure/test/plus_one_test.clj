(ns plus-one-test
  (:require [clojure.test :refer [deftest testing is]]
            [plus-one :refer [plus-one]]))

(deftest plus-one-test
  (testing "basic"
    (is (= (plus-one [1 2 3]) [1 2 4]))
    (is (= (plus-one [4 3 2 1]) [4 3 2 2]))
    (is (= (plus-one [9]) [1 0]))))
