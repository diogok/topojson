(ns topojson.writer-test
  (:use [topojson.reader :only [read-json topo2geo]])
  (:use [clojure.set])
  (:require [plumbing.core :refer :all])
  (:require [midje.sweet :refer :all]
            [topojson.writer :refer :all]))

(fact "Full convertion"
   (let [geo {:type "FeatureCollection"
              :id "example"
              :features [
                 {:type "Feature"
                  :geometry {
                     :type "Point"
                     :coordinates [0 0]}}
                 {:type "Feature"
                  :geometry {
                     :type "Polygon"
                     :coordinates [[[0 0] [0 1] [1 1] [1 0] [0 0]]]}}
                 {:type "Feature"
                  :geometry {
                     :type "MultiPolygon"
                     :coordinates 
                             [[[[0 0] [0 1] [1 1] [1 0] [0 0]]]
                               [[[1 1] [1 2] [2 2] [2 1] [1 1]]]]}}
                 {:type "Feature"
                  :geometry {
                     :type "LineString"
                     :coordinates [[0 0] [0 1] [1 1]]}}
                 {:type "Feature"
                  :geometry {
                     :type "MultiLineString"
                     :coordinates
                             [[[0 0] [0 1] [1 1]]
                              [[1 1] [1 2] [2 2]]
                              [[5 5] [6 6] [8 8]]]}}]}
         topo {:type "Topology"
               :transform {:scale [1 1] 
                           :translate [1 1]}
               :objects {
                  :example {
                    :type "GeometryCollection"
                    :id "example"
                    :geometries [
                      {
                       :type "Point"
                       :coordinates [-1 -1]
                      }
                      {
                       :type "Polygon"
                       :arcs [[0 1 2 3 0]]
                      }
                      {
                       :type "MultiPolygon"
                       :arcs [[[0 1 2 3 0]]
                              [[2 4 5 6 2]]]
                      }
                      {
                       :type "LineString"
                       :arcs [0 1 2]
                      }
                      {
                       :type "MultiLineString"
                       :arcs [[0 1 2] [2 4 5] [7]]
                      }
                    ]
                  }
               }
               :arcs [[[-1 -1]]
                      [[-1 0]]
                      [[0 0]]
                      [[0 -1]]
                      [[0 1]]
                      [[1 1]]
                      [[1 0]]
                      [[4 4] [1 1] [2 2]]]}]
     (binding [*q* false] (geo2topo geo)) => topo
     (first (:features (topo2geo topo))) => geo))

(fact "Example convertion"
  (let [ex-geo-src  (read-json (slurp "test/data/ex.geo.json"))
        ex-topo-src (read-json (slurp "test/data/ex.topo.json"))
        ex-topo-dst (binding [*type* float] (geo2topo (assoc ex-geo-src :id "example") ))
        ex-geo-dst  (first (:features (topo2geo ex-topo-dst)))]
    (dissoc-in ex-topo-dst [:objects :example :id]) => ex-topo-src
    (dissoc ex-geo-dst :id) => ex-geo-src
    (write-json "ex.topo.json" ex-topo-dst)))

