(ns topojson.writer
  (:require [plumbing.core :refer :all])
  (:require [plumbing.graph :as graph])
  (:require [com.rpl.specter :refer :all])
  (:require [topojson.reader :refer [maybe-round no-nil]])
  (:require [clojure.data.json :as json]))

(defn collect-coords
  [feat] 
  (condp = (:type feat)
    "Point"
      [[(:coordinates feat)]]
    "MultiPoint" 
      [(:coordinates feat)]
    "LineString" 
      [(:coordinates feat)]
    "MultiLineString" 
      (:coordinates feat)
    "Polygon" 
      (:coordinates feat)
    "MultiPolygon" 
      (apply concat (:coordinates feat))
    []))

(defn collect-arcs-0
  [feat] 
  (condp = (:type feat)
    "LineString" 
      [(:arcs feat)]
    "MultiLineString" 
      (:arcs feat)
    "Polygon" 
      (:arcs feat)
    "MultiPolygon" 
      (apply concat (:arcs feat))
    []))

(defn collect-arcs-1
  [feat] 
  (condp = (:type feat)
    "LineString" 
      (:arcs feat)
    "MultiLineString" 
      (apply concat (:arcs feat))
    "Polygon" 
      (apply concat (:arcs feat))
    "MultiPolygon" 
      (apply concat (apply concat (:arcs feat)))
    []))

(defn cut-arcs
  [junctions feat]
  (condp = (:type feat)
    "LineString" 
      (assoc feat :arcs (mapv vec (partition-by junctions (:arcs feat))))
    "MultiLineString" 
      (assoc feat :arcs (mapv #(mapv vec (partition-by junctions %)) (:arcs feat)))
    "Polygon" 
      (assoc feat :arcs (mapv #(mapv vec (partition-by junctions %)) (:arcs feat)))
    "MultiPolygon" 
      (assoc feat :arcs
       (mapv 
         (fn [p] (mapv #(mapv vec (partition-by junctions %)) p))
           (:arcs feat)))
    feat))

(defn extract-arcs
  [idx geo]
  (condp = (:type geo)
    "LineString" 
      (assoc geo :arcs 
        (mapv idx (:arcs geo)))
    "MultiLineString" 
      (assoc geo :arcs
        (mapv
          (fn [line] (mapv idx line))
            (:arcs geo)))
    "Polygon" 
      (assoc geo :arcs
        (mapv 
         (fn [ring] (mapv idx ring))
            (:arcs geo)))
    "MultiPolygon" 
      (assoc geo :arcs
        (mapv 
          (fn [poly]
           (mapv
             (fn [ring] 
               (mapv idx ring))
             poly))
          (:arcs geo)))
      geo))

(defn delta
  [pairs] 
    (loop [current (first pairs) rest-pairs (next pairs) x 0.0 y 0.0 dst (transient [])]
      (if (nil? current) 
        (persistent! dst)
        (let [position [(- (first current) x) 
                        (- (last current) y)]]
          (recur (first rest-pairs)
                 (next rest-pairs) 
                 (double (+ x (first position) ))
                 (double (+ y (last position)) )
                 (conj! dst (mapv maybe-round position)))))))

(defn transform-0 [transform [lng lat]]
  [(maybe-round (/ (- lng (first (:translate transform))) (first (:scale transform))))
   (maybe-round (/ (- lat (second (:translate transform))) (second (:scale transform))))])

(defn transform-sel
  [feat]
  (condp = (:type feat)
    "Point"
      [:coordinates]
    "MultiPoint" 
      [:coordinates ALL]
    "LineString" 
      [:coordinates ALL]
    "MultiLineString" 
      [:coordinates ALL ALL]
    "Polygon" 
      [:coordinates ALL ALL]
    "MultiPolygon" 
      [:coordinates ALL ALL ALL]
    []))

(defn feat-transform
  [quantum feat] 
   (transform (transform-sel feat) (partial transform-0 quantum) feat))

(defn feat2geom
  [feat] 
  (no-nil
    {:type (:type (:geometry feat))
     :id (:id feat)
     :properties (:properties feat)
     :coordinates (if (or 
                        (= (:type (:geometry feat)) "Point") 
                        (= (:type (:geometry feat)) "MultiPoint"))
                    (:coordinates (:geometry feat)))
     :arcs (if (and 
                  (not (= (:type (:geometry feat)) "Point"))
                  (not (= (:type (:geometry feat)) "MultiPoint")))
                  (:coordinates (:geometry feat)))}))

(defn geocol
  [geo] 
  (let [id (or (:id geo) (str "id" (hash geo)))]
    {(keyword id)
      (no-nil
        {:type "GeometryCollection"
         :id id
         :properties (:properties geo)
         :geometries (mapv feat2geom (:features geo))
         })}))

(def processor
 (graph/compile 
   {:type (fnk [] "Topology" )
    :all-coords-raw
      (fnk [geos] 
        (time
        (do (println "all")
          (doall
            (->> geos
                 (map :features)
                 (apply concat)
                 (map :geometry)
                 (map collect-coords)
                 (apply concat)
                 (apply concat)
                 )))))
    :quantum
      (fnk [all-coords-raw]
        (time (do (println "quantums")
         (let [lngs  (distinct-fast (map first all-coords-raw))
               lats  (distinct-fast (map second all-coords-raw))

               x0 (apply min lngs)
               y0 (apply min lats)
               x1 (apply max lngs)
               y1 (apply max lats)

               xd (- x1 x0)
               yd (- y1 y0)

               kx (if (zero? xd) 1 (/ (- 1e4 1) xd))
               ky (if (zero? yd) 1 (/ (- 1e4 1) yd))]
           {:scale  [(/ 1 kx) (/ 1 ky)]
            :translate [x0 y0]}))))
    :transform (fnk [quantum] quantum)
    :geos-transformed
      (fnk [geos quantum]
        (transform [ALL :features ALL :geometry]
          (partial feat-transform quantum)
          geos))
    :objects-raw
      (fnk [geos-transformed] 
        (apply merge
          (map geocol geos-transformed)))
    :feats-raw
      (fnk [objects-raw] 
        (apply concat
          (select [ALL LAST :geometries]
             objects-raw)))
    :all-coords
      (fnk [feats-raw]
        (time
        (do (println "all-coords")
        (doall
          (apply concat (mapv distinct (apply concat (map collect-arcs-0 feats-raw)) )))
        )
        )
        )
    :junctions
      (fnk [all-coords] 
        (time (do (println "juncs")
          (set
            (select
              [ALL #(> (last %) 1) FIRST]
              (frequencies all-coords))))))
    :objects-cut
      (fnk [objects-raw junctions]
        (time (do (println "feats-cut")
          (transform
            [ALL LAST :geometries ALL]
            (partial cut-arcs junctions)
            objects-raw)
        )))
    :arcs-raw
      (fnk [objects-cut]
        (time (do (println "arcs")
          (doall
          (->> objects-cut
            (vals)
            (mapv :geometries)
            (apply concat)
            (mapv collect-arcs-1)
            (apply concat)
            (distinct-fast)
            (vec))))))
    :arcs-idx
      (fnk [arcs-raw]
           (time (do (println "idx")
              (for-map [i (range 0 (count arcs-raw))]
                (get arcs-raw i) i))))
    :objects 
      (fnk [arcs-idx objects-cut]
          (time
          (do (println "arced")
          (transform
            [ALL LAST :geometries ALL]
            (partial extract-arcs arcs-idx)
            objects-cut
            ))
          )
          )
    :arcs 
     (fnk [arcs-raw]
        (time 
(do (println "delta")

        (mapv delta arcs-raw))
)
        )
    }))

(defn geo2topo
  [ & geos ] 
  (dissoc (processor {:geos geos})
    :topo
    :feats
    :all-arcs
    :all-raw-arcs
    :all-coords
    :all-coords-raw
    :arcs-idx
    :geos-transformed
    :quantum
    :junctions
    :objects-raw
    :feats-raw
    :objects-cut
    :feats-arced))

(defn write-json
  [dest topo] 
  (spit dest (json/write-str topo)))

