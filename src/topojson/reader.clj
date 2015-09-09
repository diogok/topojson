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

(defn decode-position 
  ([transform [x y]] (decode-position transform x y))
  ([transform x y] 
    [(maybe-round
       (+ 
         (* x (or (get-in transform [:scale 0]) 1)) 
         (or (get-in transform [:translate 0]) 0)))
     (maybe-round
       (+ 
         (* y (or (get-in transform [:scale 1]) 1)) 
          (or (get-in transform [:translate 1]) 0)))]))

(defn decode-arc
  ([topo arc] (decode-arc topo 0 0 arc []))
  ([topo x y src-arc dst-arc]
   (let [current  (first src-arc)
         position [(+ x (first current)) (+ y (second current))]]
     (if (nil? (next src-arc))
       (mapv (partial decode-position (:transform topo)) (conj dst-arc position))
       (recur topo (first position) (last position) (rest src-arc) (conj dst-arc position))))))

(defn decode-arcs
  ([topo] (decode-arcs topo (:arcs topo)))
  ([topo arcs]  (mapv (partial decode-arc topo) arcs)))

(defn get-arc 
  [arcs n]
  (if (>= n 0)
    (get arcs n)
    (get (vec (reverse arcs)) (dec (* n -1)))))

(defn get-line
  [arcs0 arcs1]
  (into [] (reduce concat [] (mapv (fn [arcn] (get-arc arcs0 arcn)) arcs1))))

(defn get-lines
  [arcs0 arcs1]
    (mapv (fn [rearcs] (get-line arcs0 rearcs)) arcs1))

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
      (mapv (fn [line] (get-lines arcs line)) (:arcs object))
    nil))

(defn topo2geo
  ([topo]
     {:type "FeatureCollection"
      :features (mapv 
                  (partial topo2geo (:transform topo) (decode-arcs topo (:arcs topo)))
                  (mapv val (:objects topo)))})
 ([transform arcs object]
   (condp = (:type object)
     "GeometryCollection"
        (no-nil
          {:type "FeatureCollection"
           :id (:id object)
           :properties (:properties object)
           :features (mapv no-nil (mapv (partial topo2geo transform arcs) (:geometries object)))})
     {:type "Feature"
      :id (:id object)
      :properties (:properties object)
      :geometry {
        :type (:type object)
        :coordinates (decode-object-coordinates transform arcs object)}}
     )))

