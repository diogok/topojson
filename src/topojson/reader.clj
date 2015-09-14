(ns topojson.reader
  (:require [clojure.data.json :as json]))

(defn read-json
  [geojson] (json/read-str geojson :key-fn keyword))

(defn can-round?
  [value] (= value (float (int value))))

(defn maybe-round
  [value] (if (can-round? value) (int value) value))

(defn no-nil
  [hashmap] (reduce merge {} (filter #(not (nil? (val %))) hashmap)))

(defn must-have-id
  [kv]
  (assoc (val kv) :id (or (:id (val kv)) (name (key kv)))))

(defn decode-position-raw
  [scale-x translate-x scale-y translate-y [x y]] 
    [(maybe-round (+ (* x scale-x) translate-x))
     (maybe-round (+ (* y scale-y) translate-y))])

(defn make-decoder
  [transform]
   (partial decode-position-raw 
     (or (get-in transform [:scale 0]) 1)
     (or (get-in transform [:translate 0]) 0)
     (or (get-in transform [:scale 1]) 1)
     (or (get-in transform [:translate 1]) 0)))

(defn decode-position 
  ([transform [x y]] (decode-position transform x y))
  ([transform x y] ((make-decoder transform) [x y])))

(defn decode-arc
  ([topo arc] 
   (decode-arc (make-decoder (:transform topo)) 0 0 arc []))
  ([decoder x y src-arc dst-arc]
   (let [current  (first src-arc)
         position [(+ x (first current)) (+ y (second current))]]
     (if (nil? (next src-arc))
       (conj dst-arc (decoder position))
       (recur decoder
        (first position) (last position) 
        (rest src-arc)   (conj dst-arc (decoder position)))))))

(defn decode-arcs
  ([topo] (decode-arcs topo (:arcs topo)))
  ([topo arcs]
   (into [] (map (partial decode-arc topo) arcs))))

(defn get-arc 
  [arcs n]
  (if (>= n 0)
    (get arcs n)
    (get arcs (+ (count arcs) n))))

(defn get-line
  [arcs0 arcs1]
  (into [] (apply concat (map (partial get-arc arcs0) arcs1))))

(defn get-lines
  [arcs0 arcs1]
    (into [] (map (partial get-line arcs0) arcs1)))

(defn decode-object-coordinates
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

