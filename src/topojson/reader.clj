(ns topojson.reader
  (:require [clojure.data.json :as json]))

(defn read-json
  "Read json"
  [geojson] (json/read-str geojson :key-fn keyword))

(defn can-round?
  "Check if this number can be made round (an int) without loosing data"
  [value] (= value (Math/rint value)))

(defn maybe-round
  "Round a number if it will not loose data"
  [value] (if (can-round? value) (int value) value))

(defn no-nil
  "Remove nil properties"
  [hashmap] (reduce merge {} (filter #(not (nil? (val %))) hashmap)))

(defn must-have-id
  "Ensure kv have an id"
  [kv]
  (assoc (val kv) :id (or (:id (val kv)) (name (key kv)))))

(defn decode-position-raw
  "Raw decoder (transalte scale) of a point"
  [[^double scale-x ^double translate-x ^double scale-y ^double translate-y] [x y]] 
    [(maybe-round (+ (* x scale-x) translate-x))
     (maybe-round (+ (* y scale-y) translate-y))])

(defn make-decoder
  "Create decoder function for transformation"
  [transform]
   (partial decode-position-raw 
     [(or (get-in transform [:scale 0]) 1.0)
     (or (get-in transform [:translate 0]) 0.0)
     (or (get-in transform [:scale 1]) 1.0)
     (or (get-in transform [:translate 1]) 0.0)]))

(defn decode-position 
  "Decode a position (translate and scale)"
  ([transform [^double x ^double y]] (decode-position transform x y))
  ([transform ^double x ^double y] ((make-decoder transform) [x y])))

(defn decode-arc
  "Decode a whole ARC, undo it's delta"
  [[^double scale-x ^double scale-y ^double translate-x ^double translate-y] arc]
  (loop [current (first arc) arc (rest arc) x 0.0 y 0.0 dst (transient [])]
    (if (nil? current) 
      (persistent! dst)
      (let [position [(+ x (first current)) 
                      (+ y (second current))]]
        (recur (first arc)
               (rest arc) 
               (double (first position))
               (double (last position))
               (conj! dst 
                [(maybe-round (+ (* (first position) scale-x) translate-x)) 
                 (maybe-round (+ (* (second position) scale-y) translate-y))]))))))

(defn decode-arcs
  "Decode several arcs"
  ([topo] (decode-arcs topo (:arcs topo)))
  ([topo arcs]
   (into []
     (map
       (partial decode-arc 
                [(or (first (:scale (:transform topo))) 1.0)
                 (or (second (:scale (:transform topo))) 1.0)
                 (or (first (:translate (:transform topo))) 0.0)
                 (or (second (:translate (:transform topo))) 0.0)])
       arcs))))

(defn get-arc 
  "Get the arc number, as it maybe reversed"
  [arcs n]
  (if (>= n 0)
    (get arcs n)
    (get arcs (+ (count arcs) n))))

(defn get-line
  "Get all arcs in line"
  [arcs0 arcs1]
  (into [] (apply concat (map (partial get-arc arcs0) arcs1))))

(defn get-lines
  "Get all arcs in all lines"
  [arcs0 arcs1]
    (into [] (map (partial get-line arcs0) arcs1)))

(defn decode-object-coordinates
  "Decode the lines or rings in depth of the object "
  [transform arcs object] 
  (condp = (:type object)
    "Point"
      (decode-position transform (:coordinates object))
    "MultiPoint" 
       (mapv (partial decode-position transform) (:coordinates object))
    "LineString" 
      (get-line arcs (:arcs object))
    "MultiLineString" 
      (get-lines arcs (:arcs object))
    "Polygon" 
      (get-lines arcs (:arcs object))
    "MultiPolygon" 
      (into [] (map (partial get-lines arcs) (:arcs object)))
    nil))

(defn topo2geo
  "Converts a topojson into a geojson"
  ([topo]
   {:type "FeatureCollection"
    :features (into []
                (map 
                  (partial topo2geo (:transform topo) (decode-arcs topo (:arcs topo)))
                  (mapv must-have-id (:objects topo))))})
  ([transform arcs object]
   (condp = (:type object)
     "GeometryCollection"
        (no-nil
          {:type "FeatureCollection"
           :id (:id object)
           :properties (:properties object)
           :features (into [] (map (comp no-nil (partial topo2geo transform arcs)) (:geometries object)))})
     {:type "Feature"
      :id (:id object)
      :properties (:properties object)
      :geometry {
        :type (:type object)
        :coordinates (decode-object-coordinates transform arcs object)}})))

