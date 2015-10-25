(ns topojson.writer
  (:require [plumbing.core :refer :all])
  (:require [plumbing.graph :as graph])
  (:require [com.rpl.specter :refer :all])
  (:require [topojson.reader :refer [maybe-round no-nil]])
  (:require [clojure.data.json :as json]))

(def ^:dynamic *q* 1e4)
(def ^:dynamic *type* float)

(defn collect-coords
  "Collect all coordinates from a feature, acording to its type."
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
  "Collect all arcs from a feature, but keep the depth of it."
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
  "Collect all arcs from a feature and flatten it."
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

(def sels
  {"LineString" (comp-paths :arcs)
   "MultiLineString" (comp-paths :arcs ALL)
   "Polygon" (comp-paths :arcs ALL)
   "MultiPolygon" (comp-paths :arcs ALL ALL)})

(def sels-2
  {"LineString" (comp-paths :arcs ALL)
   "MultiLineString" (comp-paths :arcs ALL ALL)
   "Polygon" (comp-paths :arcs ALL ALL)
   "MultiPolygon" (comp-paths :arcs ALL ALL ALL)})

(def arc-sel
  (comp-paths [ALL LAST :geometries ALL]))

(def geos-sel
  (comp-paths [ALL :features ALL :geometry]))

(def arc-sel
  (comp-paths [ALL LAST :geometries ALL]))

(defn collect-arcs-2
  "Alternative to collect-arcs-1 to collect flatten arcs."
  [feat]
   (if (:arcs feat)
     (compiled-select (sels-2 (:type feat)) feat)
     []))

(defn cut-arcs
  "Cut the arcs of the feature on the junctions"
  [junctions feat]
  (if (:arcs feat)
    (compiled-transform 
      (sels (:type feat))
      (partial partition-by junctions)
      feat)
    feat))

(defn extract-arcs
  "Maps the arcs of the features to the index of arcs."
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
  "Make the arc line/ring a delta."
  [pairs] 
    (loop [current (first pairs) rest-pairs (next pairs) x 0.0 y 0.0 dst (transient [])]
      (if (nil? current) 
        (persistent! dst)
        (let [position [(- (first current) x) 
                        (- (last current) y)]]
          (recur (first rest-pairs)
                 (next rest-pairs) 
                 (double (+ x (first position)))
                 (double (+ y (last position)))
                 (conj! dst (mapv maybe-round position)))))))

(defn transform-0 [transform [lng lat]]
  "Transform (scale and translate) a point."
  [(maybe-round (*type* (/ (- lng (first (:translate transform))) (first (:scale transform)))) )
   (maybe-round (*type* (/ (- lat (second (:translate transform))) (second (:scale transform)))))])  

(def transform-sel
  {
    "Point"
      (comp-paths :coordinates)
    "MultiPoint" 
      (comp-paths :coordinates ALL)
    "LineString" 
      (comp-paths :coordinates ALL)
    "MultiLineString" 
      (comp-paths :coordinates ALL ALL)
    "Polygon" 
      (comp-paths :coordinates ALL ALL)
    "MultiPolygon" 
      (comp-paths :coordinates ALL ALL ALL)
  })

(defn feat-transform
  "Transform (scale and translate) a feature"
  [quantum feat] 
   (compiled-transform 
     (transform-sel (:type feat))
     (partial transform-0 quantum)
     feat))


(defn feat2geom
  [feat] 
  "Convert a GeoJSON feature to a TopoJSON geometry base structure"
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

(def processor
 (graph/compile 
   {:type (fnk [] "Topology" )
    :all-coords-raw
      (fnk [geos] 
        (->> geos
           (select [ALL :features ALL :geometry])
           (map collect-coords)
           (apply concat)
           (map distinct-fast)
           (apply concat)))
    :quantum
      (fnk [all-coords-raw]
        (if (not *q*)
          {:scale  [ 1 1]
           :translate [1 1]}
         (let [lngs  (distinct-fast (map first all-coords-raw))
               lats  (distinct-fast (map second all-coords-raw))

               x0 (apply min lngs)
               y0 (apply min lats)
               x1 (apply max lngs)
               y1 (apply max lats)

               xd (- x1 x0)
               yd (- y1 y0)

               qd- (- *q* 1)

               qd (if (zero? qd-) 1 qd-)

               kx (if (zero? xd) 1 (/ qd xd))
               ky (if (zero? yd) 1 (/ qd yd))]
             {:scale  [(/ 1 kx) (/ 1 ky)]
              :translate [x0 y0]}
             )
           ))
    :transform (fnk [quantum] quantum)
    :geos-transformed
      (fnk [geos quantum]
        (compiled-transform geos-sel
          (partial feat-transform quantum)
          geos))
    :objects-raw
      (fnk [geos-transformed] 
        (for-map [geo geos-transformed]
          (keyword (or (:id geo) (str "geo" (hash geo))))
          (no-nil
            {:type "GeometryCollection"
             :id (or (:id geo) (str "geo" (hash geo))) 
             :properties (:properties geo)
             :geometries (mapv feat2geom (:features geo))
             })))
    :all-coords
      (fnk [objects-raw]
        (->> objects-raw
          (compiled-select arc-sel)
          (map collect-arcs-2)
          (map distinct-fast)
          (apply concat)))
    :junctions
      (fnk [all-coords] 
        (->> all-coords
           (frequencies)
           (filter #(> (val %) 1))
           (keys)
           (set)))
    :objects-cut
      (fnk [objects-raw junctions]
        (transform
          [ALL LAST :geometries ALL]
          (partial cut-arcs junctions)
          objects-raw))
    :arcs-raw
      (fnk [objects-cut]
        (->> objects-cut
          (compiled-select arc-sel)
          (map collect-arcs-2)
          (apply concat)
          (distinct-fast)
          (vec)))
    :arcs-idx
      (fnk [arcs-raw]
        (for-map [i (range 0 (count arcs-raw))]
          (get arcs-raw i) i))
    :objects 
      (fnk [arcs-idx objects-cut]
          (transform
            [ALL LAST :geometries ALL]
            (partial extract-arcs arcs-idx)
            objects-cut))
    :arcs 
     (fnk [arcs-raw]
        (mapv delta arcs-raw))
    }))

(defn geo2topo
  "Convert all geojson to a single topojson"
  [ & geos ] 
  (dissoc (processor {:geos geos})
    :all-coords-raw
    :quantum
    :geos-transformed
    :objects-raw
    :all-coords
    :junctions
    :objects-cut
    :arcs-raw
    :arcs-idx))

(defn write-json
  "Write json to file dest"
  [dest topo] 
  (spit dest (json/write-str topo)))

