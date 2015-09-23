(ns topojson.writer-test
  (:use [topojson.reader :only [read-json topo2geo]])
  (:require [midje.sweet :refer :all]
            [topojson.writer :refer :all]))

(fact "Can extract few arcs"
  (extract-arcs
   {:type "Feature"
    :geometry {
       :type "Point"
       :coordinates [0 0]}})
    => []
  (extract-arcs
   {:type "Feature"
    :geometry {
       :type "Polygon"
       :coordinates [[[0 0] [0 1] [1 1] [1 0] [0 0]]]}})
    => [[[0 0] [0 1] [1 1] [1 0] [0 0]]]
  (extract-arcs
   {:type "Feature"
    :geometry {
       :type "MultiPolygon"
       :coordinates 
               [[[[0 0] [0 1] [1 1] [1 0] [0 0]]]
                 [[[1 1] [1 2] [2 2] [2 1] [1 1]]]]}})
      => [[[0 0] [0 1] [1 1] [1 0] [0 0]]
          [[1 1] [1 2] [2 2] [2 1] [1 1]]]
  (extract-arcs
   {:type "Feature"
    :geometry {
       :type "LineString"
       :coordinates [[0 0] [0 1] [1 1]]}})
      => [[[0 0] [0 1] [1 1]]]
  (extract-arcs
   {:type "Feature"
    :geometry {
       :type "MultiLineString"
       :coordinates
               [[[0 0] [0 1] [1 1]]
                [[1 1] [1 2] [2 2]]]}})
      => [[[0 0] [0 1] [1 1]]
          [[1 1] [1 2] [2 2]]])

(fact "Can extract lines and ring from a collection"
   (extract-arcs
     {:type "FeatureCollection"
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
                      [[1 1] [1 2] [2 2]]]}}
                        ]})
      =>
        [
         [[0 0] [0 1] [1 1] [1 0] [0 0]]
         [[1 1] [1 2] [2 2] [2 1] [1 1]]
         [[0 0] [0 1] [1 1]]
         [[1 1] [1 2] [2 2]]])

(fact "Cut arcs at the junctions"
  (vec
  (cut-at-junctions
    [
     [[0 0] [0 1] [1 1] [1 0] [0 0]]
     [[1 1] [1 2] [2 2] [2 1] [1 1]]
     [[0 0] [0 1] [1 1]]
     [[1 1] [1 2] [2 2]]
     [[1 1] [1.5 1.5] [1.7 1.7] [2 2]]
     [[5 5] [6 6] [7 7] [5 5]]])
  )
  => [[[0 0]]
      [[0 1]]
      [[1 1]]
      [[1 0]]
      [[1 2]]
      [[2 2]]
      [[2 1]]
      [[1.5 1.5] [1.7 1.7]]
      [[5 5] [6 6] [7 7] [5 5]]])

#_(fact "Nice translation and scalation"
  (translate
    [[[15 15] [10 10] [20 20] [15 15]]
     [[12 12]]
     [[25 25] [30 30]]])
  => {
      :translate [15 15]
      :arcs [[[0 0] [-5 -5] [5 5] [0 0]]
             [[-3 -3]]
             [[10 10] [15 15]]]}
  (transform-0
    [[[15 15] [10 10] [20 20] [15 15]]
     [[12 12]]
     [[25 25] [30 30]]])
  => {
      :transform { :translate [15 15]
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

(fact "Finding arcs"
   (let [arcs [[[0 0]]
               [[0 1]]
               [[1 1]]
               [[1 0]]
               [[1 2]]
               [[2 2]]
               [[2 1]]
               [[1.5 1.5] [1.7 1.7]]
               [[5 5] [6 6] [7 7] [5 5]]]]
     (get-arcs-0 arcs
       [[0 0] [0 1] [1 1] [1 0] [0 0]])
       => [0 1 2 3 0]
     (get-arcs-0 arcs
       [[1 1] [1 2] [2 2] [2 1] [1 1]])
      => [2 4 5 6 2]
     (get-arcs-0 arcs
       [[0 0] [0 1] [1 1]])
      => [0 1 2]
     (get-arcs-0 arcs
       [[1 1] [1.5 1.5] [1.7 1.7] [2 2]])
      => [2 7 5]
     (get-arcs-0 arcs
       [[5 5] [6 6] [7 7] [5 5]])
      => [8]))

#_(fact "Full convertion"
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
                       :arcs [ [0 1 2] [2 4 5]]
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
     (first (:features (topo2geo topo))) => geo
   ))

(def ex-geo (assoc (read-json (slurp (clojure.java.io/resource "test/ex.geo.json"))) :id "example"))
(def ex-topo (read-json (slurp (clojure.java.io/resource "test/ex.topo.json"))))

#_(fact "Example convertion"
  (first (:features (topo2geo (geo2topo ex-geo)))) => ex-geo)

#_(fact "Bigger convertion"
  (let [ucs-topo   (read-json (slurp (clojure.java.io/resource "test/ucs.json")))
        ti-topo    (read-json (slurp (clojure.java.io/resource "test/ti.json")))
        both-topo  (read-json (slurp (clojure.java.io/resource "test/both.json")))
        [ti-geo ucs-geo]  (:features (topo2geo both-topo) )]
    (time
    (geo2topo ti-geo)
    )
    ))

