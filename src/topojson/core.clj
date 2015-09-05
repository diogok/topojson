(ns topojson.core
  (:require [clojure.data.json :as json]))

(defn read-geojson
  [geojson] (json/read-str geojson :key-fn keyword))

(defn decode-position 
  ([topo [x y]] (decode-position topo x y))
  ([topo x y] 
    [(+ 
       (* x (or (get-in topo [:transform :scale 0]) 1)) 
       (or (get-in topo [:transform :translate 0]) 0))
     (+ 
       (* y (or (get-in topo [:transform :scale 1]) 1)) 
        (or (get-in topo [:transform :translate 1]) 0))]))

(defn decode-arc
  ([topo arc] (decode-arc topo 0 0 arc []))
  ([topo x y src-arc dst-arc]
   (let [current  (first src-arc)
         position [(+ x (first current)) (+ y (second current))]]
     (if (nil? (next src-arc))
       (vec (map (partial decode-position topo) (conj dst-arc position)))
       (recur topo (first position) (last position) (rest src-arc) (conj dst-arc position))))))

(defn decode-arcs
  ([topo] (decode-arcs topo (:arcs topo)))
  ([topo arcs] (vec (map (partial decode-arc topo) arcs)) ))

(defn get-arc 
  [arcs n]
  (if (>= n 0)
    (get arcs n)
    (get (vec (reverse arcs)) (dec (* n -1)))))

(defn decode-object-coordinates
  [arcs object] 
  (condp = (:type object)
    "Point"
      (:coordinates object)
    "MultiPoint" 
      (:coordinates object)
    "LineString" 
      (vec (reduce concat [] (vec (map (fn [arcn] (get-arc arcs arcn)) (:arcs object)))))
    "MultiLineString" nil
    "Polygon"  nil
    "MultiPolygon" nil
    nil))

(defn to-geo
  ([topo] (to-geo topo topo))
  ([topo object] 
   (condp = (:type object)
     "Topology"
        {:type "FeatureCollection"
         :properties (:properties object)
         :id (:id object)
         :features (map (partial to-geo topo) (map val (:objects object)))}
     "GeometryCollection"
        {:type "FeatureCollection"
         :properties (:properties object)
         :id (:id object)
         :features (map (partial to-geo topo) (:geometries object))}
     {:type "Feature"
      :properties (:properties object)
      :id (:id object)
      :geometry {
        :type (:type object)
        :coordinates (decode-object-coordinates (decode-arcs topo (:arcs topo)) object)}})))

