(ns topojson.writer-test
  (:use [topojson.reader :only [read-json]])
  (:require [clojure.test :refer :all]
            [topojson.writer :refer :all]))

(deftest prepare
  (testing "wahtno"
    (is 
      (= [] (list)))))

(comment
(deftest convert
  (testing "converting"
    (is 
      (=
       (read-json (slurp (clojure.java.io/resource "ex.topo.json")))
       (geo2topo 
         (assoc (read-json (slurp (clojure.java.io/resource "ex.geo.json"))) :id "example"))))))
)

