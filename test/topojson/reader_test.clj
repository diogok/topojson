(ns topojson.reader-test
  (:require [midje.sweet :refer :all]
            [clojure.test :refer :all]
            [topojson.reader :refer :all]))

(facts "Decoding positions"

  (fact "Decoding position"
    (decode-position {} 3 4)
    =>
    [3 4])

  (fact "Decoding with transformation"
    (decode-position {:scale [2 2] :translate [2 2]} 3 3)
    =>
    [8 8]
    (decode-position {:scale [2 1] :translate [8 2]} 3 4)
    =>
    [14 6])

  (fact "Decoding arcs"
    (is 
      (= 
        (decode-arcs {} [[[0 0]]])
        [[[0 0]]]))
    (is 
      (= 
        (decode-arcs {} [[[1 1]]])
        [[[1 1]]]))
    (is 
      (= 
        (decode-arcs {} [[[1 1] [2 2] [3 3]]])
        [[[1 1] [3 3] [6 6]]]))
    (is 
      (= 
        (decode-arcs {} [[[1 1] [2 2] [3 3]]
                         [[2 1] [5 4] [-3 1]]])
        [[[1 1] [3 3] [6 6]]
         [[2 1] [7 5] [4 6]]]))))
  
(facts "decode-coordinates"

  (fact "Points stay the same"
    (is 
      (= [10 10]
         (decode-object-coordinates {} [] {:type "Point" :coordinates [10,10]})))
    (is 
      (= [[10 10] [20 20]]
         (decode-object-coordinates {} [] {:type "MultiPoint" :coordinates [[10 10][20 20]]}))))

  (fact "Simple LineStrings"
    (is 
      (= [[1 1] [6 8]]
         (decode-object-coordinates {}
           (decode-arcs {} [[[1 1] [5 7]]
                            [[2 2] [4 5]]])
           {:type "LineString" :arcs [0]}))))

  (fact "Simple LineStrings with more than one arc"
    (is 
      (= [[1 1] [6 8] [2 2] [6 7]]
         (decode-object-coordinates {}
           (decode-arcs {} [[[1 1] [5 7]]
                            [[2 2] [4 5]]])
           {:type "LineString" :arcs [0 1]}))))

  (fact "Multiple Line String"
    (is 
      (= [[[1 1] [6 8] [2 2] [6 7]]
          [[2 2] [6 7]]]
         (decode-object-coordinates {}
           (decode-arcs {} [[[1 1] [5 7]]
                            [[2 2] [4 5]]])
           {:type "MultiLineString" :arcs [[0 1] [1]]}))))

  (fact "Simple Polygon, with two arcs and with a hole"
    (is 
      (= [[[1 1] [6 8] [1 1]]]
         (decode-object-coordinates {}
           (decode-arcs {} [[[1 1] [5 7] [-5 -7]]])
           {:type "Polygon" :arcs [[0]]})))

    (is 
      (= [[[1 1] [6 8] [2 2] [1 1]]]
         (decode-object-coordinates {}
           (decode-arcs {} [[[1 1] [5 7]]
                            [[2 2] [-1 -1]]])
           {:type "Polygon" :arcs [[0 1]]})))

    (is 
      (= [[[1 1] [6 8] [2 2] [1 1]]
          [[1.2 1.2] [1.4 1.4] [1.2 1.2]]]
         (decode-object-coordinates {}
           (decode-arcs {} [[[1 1] [5 7]]
                            [[2 2] [-1 -1]]
                            [[1.2 1.2] [0.2 0.2] [-0.2 -0.2]]])
           {:type "Polygon" :arcs [[0 1] [2]]}))))

  (fact "Multipolygon"
    (is 
      (= [[[[1 1] [6 8] [1 1]]]
          [[[2 2] [3 3] [2 2]]]]
         (decode-object-coordinates {}
           (decode-arcs {} [[[1 1] [5 7] [-5 -7]]
                            [[2 2] [1 1] [-1 -1]]])
           {:type "MultiPolygon" :arcs [[[0]] [[1]]]})))))

(facts "decode-full-topology"
  (fact "Full decoding"
    (is 
      (=
       {:type "FeatureCollection"
        :features [(assoc (read-json (slurp "test/data/ex.geo.json")) :id "example")]}
       (topo2geo (read-json (slurp "test/data/ex.topo.json")))))))

