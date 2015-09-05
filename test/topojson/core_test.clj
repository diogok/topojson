(ns topojson.core-test
  (:require [clojure.test :refer :all]
            [topojson.core :refer :all]))

(deftest decoding

  (testing "Decoding position"
    (is
      (=
        (decode-position {} 3 4)
        [3 4]))
    (is
      (=
        (decode-position {:transform {:scale [2 2] :translate [2 2]}} 3 3)
        [8 8]))
    (is
      (=
        (decode-position {:transform {:scale [2 1] :translate [8 2]}} 3 4)
        [14 6])))

  (testing "Decoding arcs"
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
                         [[2 1] [5 4] [-3 1]]
                         ])
        [[[1 1] [3 3] [6 6]]
         [[2 1] [7 5] [4 6]]])))
  
  (testing "Decoding objects coordinates"
    (is 
      (= [10 10]
         (decode-object-coordinates [] {:type "Point" :coordinates [10,10]})))
    (is 
      (= [[ 10 10 ][20 20]]
         (decode-object-coordinates [] {:type "MultiPoint" :coordinates [[10 10][20 20]]})))
    (is 
      (= [[1 1] [6 8]]
         (decode-object-coordinates 
           (decode-arcs {} [[[1 1] [5 7]]
                            [[2 2] [4 5]]])
           {:type "LineString" :arcs [0]})))
    (is 
      (= [[1 1] [6 8] [2 2] [6 7]]
         (decode-object-coordinates 
           (decode-arcs {} [[[1 1] [5 7]]
                            [[2 2] [4 5]]])
           {:type "LineString" :arcs [0 1]})))
    )
  )

