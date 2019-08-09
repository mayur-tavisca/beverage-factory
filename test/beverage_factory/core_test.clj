(ns beverage-factory.core-test
  (:require [clojure.test :refer :all]
            [beverage-factory.core :refer [place-order]]))

(deftest single-order-without-customization
  (testing " Test for single order without any customization"
    (is (= 4 (place-order "Chai")))))

(deftest single-order-with-customization
  (testing " Test for single order with customization"
    (is (= 3.5 (place-order "Chai, -sugar")))))

(deftest single-order-with-invalid-customization
  (testing "Test for single order with invalid customization"
    (is (thrown? Exception (place-order "Chai,  -soda")))))

(deftest single-order-with-all-exclusions
  (testing "Test for single order with customization containing all the exclusions"
    (is (thrown? Exception (place-order "Chai,-milk,-sugar,-water")))))

(deftest multiple-order
  (testing "Multiple Order Test")
  (is (=[3.5 4 4] (place-order ["Chai, -sugar", "Chai", "Coffee, -milk"] ))))
