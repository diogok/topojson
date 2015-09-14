(ns topojson.writer-test
  (:use [topojson.reader :only [read-json topo2geo]])
  (:require [clojure.test :refer :all]
            [topojson.writer :refer :all]))

(deftest prepare
  (testing "wahtno"
    (is 
      (= [] (list)))))

(comment
(deftest convert
  (let [ucs-topo   (read-json (slurp (clojure.java.io/resource "test/ucs.json")))
        ti-topo    (read-json (slurp (clojure.java.io/resource "test/ti.json")))
        both-topo  (read-json (slurp (clojure.java.io/resource "test/both.json")))
        [ucs-geo ti-geo]  (topo2geo both-topo)]
    (comment "Something something")
    )))

