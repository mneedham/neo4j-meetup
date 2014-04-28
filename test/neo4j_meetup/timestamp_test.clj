(ns neo4j-meetup.timestamp-test
  (:require [clojure.test :refer :all]
            [neo4j-meetup.timestamp :refer :all]))

(deftest format-time
  (testing "formats time properly"
    (is (= (as-time 1309368600000) "17:30"))))

(deftest format-day
  (testing "create the appropriate suffix"
    (is (= (day-suffix 5) "th"))
    (is (= (day-suffix 3) "rd"))
    (is (= (day-suffix 2) "nd"))
    (is (= (day-suffix 1) "st"))
    (is (= (day-suffix 31) "st"))))

(deftest format-date-time
  (testing "formats date-time properly"
    (is (= (as-date-time 1309368600000) "29th June 2011, 17:30"))))

(deftest format-date
  (testing "formats date properly"
    (is (= (as-date 1309368600000) "29th June 2011"))))
