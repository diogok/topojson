(ns topojson.all-test
  (:use [topojson.reader :only [read-json topo2geo topo2geo-clj]])
  (:use [clojure.set])
  (:require [plumbing.core :refer :all])
  (:require [midje.sweet :refer :all]
            [topojson.writer :refer :all]))

(fact "Example convertion"
  (let [ex-3-topo  (read-json (slurp "test/data/br_admin_level_3.topo.json"))
        ex-4-topo  (read-json (slurp "test/data/br_admin_level_4.topo.json"))
        ex-3-geo   (topo2geo ex-3-topo)
        ex-4-geo   (topo2geo ex-4-topo)]
    (write-json "al3.geo.json" (first (:features ex-3-geo)))
    (write-json "al4.geo.json" (first (:features ex-4-geo)))
    ))

(fact "Example convertion clj"
  (let [ex-3-geo  (read-json (slurp "test/data/br_admin_level_3.geo.json"))
        ex-4-geo  (read-json (slurp "test/data/br_admin_level_4.geo.json"))]
    (time (write-json "clj.topo.json" (geo2topo-clj ex-3-geo ex-4-geo)))
    (time (write-json "js.topo.json"  (geo2topo-js  ex-3-geo ex-4-geo)))
    ))
