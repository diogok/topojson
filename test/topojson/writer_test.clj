(ns topojson.writer-test
  (:use [topojson.reader :only [read-json topo2geo]])
  (:use [clojure.set])
  (:require [midje.sweet :refer :all]
            [topojson.writer :refer :all]))

(fact "Can extract a few arcs"
  (feat2geom
   {:type "Feature"
    :geometry {
       :type "Point"
       :coordinates [0 0]}})
    => {:type "Point"
        :coordinates [0 0]}
  (feat2geom
   {:type "Feature"
    :geometry {
       :type "Polygon"
       :coordinates [[[0 0] [0 1] [1 1] [1 0] [0 0]]]}})
    => {:type "Polygon"
        :arcs [[[0 0] [0 1] [1 1] [1 0] [0 0]]]}

  (extract-0
     [{:type "Feature"
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
                   [[[1 1] [1 2] [2 2] [2 1] [1 1]]]]}}])
    =>
       [{:type "Point"
          :coordinates [0 0]}
        {:type "Polygon"
         :arcs [[[0 0] [0 1] [1 1] [1 0] [0 0]]]}
        {:type "MultiPolygon"
         :arcs [[[[0 0] [0 1] [1 1] [1 0] [0 0]]]
                [[[1 1] [1 2] [2 2] [2 1] [1 1]]]]}])

(fact "Identify junctions"
  (junctions
    [[[0 0] [0 1] [1 1] [1 0] [0 0]]
     [[1 1] [1 2] [2 2] [2 1] [1 1]]
     [[0 0] [0 1] [1 1]]
     [[1 1] [1 2] [2 2]]
     [[1 1] [1.5 1.5] [1.7 1.7] [2 2]]
     [[5 5] [6 6] [7 7] [5 5]]])
  => 
     (set
      [[0 0]
      [0 1]
      [1 1]
      [1 2]
      [2 2]]))

(fact "Can cut the arcs"
  (cut
    {:objects 
     {:example 
      {:geometries [
        {:type "Polygon"
         :arcs [[[0 0] [0 1] [1 1] [1 0] [0 0]]]}
        {:type "MultiPolygon"
         :arcs [[[[0 0] [0 1] [1 1] [1 0] [0 0]]]
                [[[1 1] [1 2] [2 2] [2 1] [1 1]]]]}
      ]}}})
  =>
    {:objects 
     {:example 
      {:geometries [
        {:type "Polygon"
         :arcs [[[[0 0] [0 1]] [[1 1]] [[1 0] [0 0]]]]}
        {:type "MultiPolygon"
         :arcs [[[[[0 0] [0 1]] [[1 1]] [[1 0] [0 0]]]]
                [[[[1 1]] [[1 2] [2 2] [2 1]] [[1 1]]]]]}
      ]}}})

(fact "Extract the arcs"
   (extract-out
    {:objects 
     {:example 
      {:geometries [
        {:type "Polygon"
         :arcs [[[[0 0] [0 1]] [[1 1]] [[1 0] [0 0]]]]}
        {:type "MultiPolygon"
         :arcs [[[[[0 0] [0 1]] [[1 1]] [[1 0] [0 0]]]]
                [[[[1 1]] [[1 2] [2 2] [2 1]] [[1 1]]]]]}
      ]}}})
    => 
    {:arcs [[[0 0] [0 1]]
            [[1 1]]
            [[1 0] [0 0]]
            [[1 2] [2 2] [2 1]]]
     :objects 
     {:example 
      {:geometries [
        {:type "Polygon"
         :arcs [[0 1 2]]}
        {:type "MultiPolygon"
         :arcs [[[0 1 2]]
                [[1 3 1]]]}
      ]}}})

(fact "Translation and scalation"
  (translate
    {:arcs
      [[[15 15] [10 10] [20 20] [15 15]]
       [[12 12]]
       [[25 25] [30 30]]]})
  => {:transform {:translate [15 15]}
      :arcs [[[0 0] [-5 -5] [5 5] [0 0]]
             [[-3 -3]]
             [[10 10] [15 15]]]}
  (scale
     {:transform {:translate [15 15]}
      :arcs [[[0 0] [-5 -5] [5 5] [0 0]]
             [[-3 -3]]
             [[10 10] [15 15]]]})
  => {:transform { :translate [15 15]
                   :scale [5 5] }
      :arcs [[[0 0] [-1 -1] [1 1] [0 0]]
             [[-3/5 -3/5]]
             [[2 2] [3 3]]]})

(fact "Squentialize"
 (sequentialize-0
    [[[0 0] [-1 -1] [1 1] [0 0]]
     [[-3/5 -3/5]]
     [[2 2] [3 3]]])
  =>[[[0 0] [-1 -1] [2 2] [-2 -2]]
     [[-0.6 -0.6]]
     [[2 2] [1 1]]])

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
                              [[1 1] [1 2] [2 2]]]}}]}
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
                       :arcs [[0 1 2] [2 4 5]]
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
                      [[1 0]]]}]
     (geo2topo geo) => topo
     (first (:features (topo2geo topo))) => geo))

(fact "Example convetion"
  (let [ex-geo-src  (read-json (slurp (clojure.java.io/resource "test/ex.geo.json")))
        ex-topo-src (read-json (slurp (clojure.java.io/resource "test/ex.topo.json")))
        ex-topo-dst (geo2topo (assoc ex-geo-src :id "example"))
        ex-geo-dst (first (:features (topo2geo ex-topo-dst)))]
    (comment ex-topo-dst => ex-topo-src)
    (dissoc ex-geo-dst :id) => ex-geo-src
    ))

(fact "Bigger convertion "
  (let [ti-topo    (read-json (slurp (clojure.java.io/resource "test/ti.json")))
        [ti-geo]   (:features (topo2geo ti-topo))]
    (topo2geo
    (time
      (geo2topo ti-geo) 
    )
    ) 
    ))

