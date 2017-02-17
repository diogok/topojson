(ns topojson.all-test
  (:use [topojson.reader :only [read-json topo2geo]])
  (:use [clojure.set])
  (:require [plumbing.core :refer :all])
  (:require [midje.sweet :refer :all]
            [topojson.writer :refer :all]))

(fact "Example convertion"
  (let [ex-uc-topo  (read-json (slurp "test/data/ucs.topo.json"))
        ex-ti-topo  (read-json (slurp "test/data/ti.topo.json"))
        ti-geo      (topo2geo ex-ti-topo)
        uc-geo      (topo2geo ex-uc-topo)]
    (write-json "ti.geo.json" (first (:features ti-geo)))
    (write-json "uc.geo.json" (first (:features uc-geo)))
    ))
