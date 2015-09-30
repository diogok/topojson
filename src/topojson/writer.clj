(ns topojson.writer
  (:require [topojson.reader :refer [maybe-round no-nil]])
  (:require [clojure.data.json :as json])
  (:require [clojure.set :refer :all]))

(defn run
  [fun k obj] 
  {(key obj) (assoc (val obj) k (fun (k (val obj))))})

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

(defn junctions
  [arcs] 
  (set
  (map first (filter #(> (val %) 1) (frequencies (apply concat arcs))))
  ))

(defn cut-arcs
  [junctions feat]
  (condp = (:type feat)
    "LineString" 
      (assoc feat :arcs (mapv vec (partition-by junctions (:arcs feat)) ))
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

(defn cut-0
  [cutter geo]
  {(key geo)
   (assoc (val geo) :geometries
     (mapv cutter (:geometries (val geo))))})

(defn cut 
  [topo] 
   (let [feats     (apply concat (map :geometries (vals (topo :objects))))
         raw-arcs  (apply concat (map collect-arcs-0 feats))
         cutter    (partial cut-arcs (time (junctions (distinct raw-arcs))))]
     (assoc topo :objects
       (apply merge
         (map (partial cut-0 cutter) (:objects topo))))))

(defn extract-2
  [idx geo]
  (condp = (:type geo)
    "LineString" 
      (assoc geo :arcs 
        (idx (:arcs geo)))
    "MultiLineString" 
      (assoc geo :arcs
        (mapv idx (:arcs geo)))
    "Polygon" 
      (assoc geo :arcs
        (mapv idx (:arcs geo)))
    "MultiPolygon" 
      (assoc geo :arcs
        (mapv (partial mapv idx) (:arcs geo)))
      geo))

(defn extract-1
  [arcs geos]
  (mapv (partial extract-2 (partial mapv arcs)) geos))

(defn extract-out
  [topo] 
   (let [feats     (apply concat (map :geometries (vals (topo :objects))))
         raw-arcs  (distinct (apply concat (map collect-arcs-1 feats))) 
         arc-idx   (apply merge (map-indexed #(hash-map %2 %1) raw-arcs))]
     (assoc topo
       :arcs raw-arcs
       :objects
       (apply merge
         (map (partial run (partial extract-1 arc-idx) :geometries) (:objects topo))))))

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

(defn extract-0
  [geo] 
  (mapv feat2geom geo))

(defn extract-in
  [topo] 
  (assoc topo :objects
    (apply merge
       (map
         (partial run extract-0 :geometries)
         (:objects topo)))))

(defn sequentialize-0
  [arcs] 
  (mapv 
    (fn [arc]
      (loop [current (first arc) arc (rest arc) x 0.0 y 0.0 dst (transient [])]
        (if (nil? current) 
          (persistent! dst)
          (let [position [(- (first current) x) 
                          (- (second current) y)]]
            (recur (first arc)
                   (rest arc) 
                   (double (first position))
                   (double (last position))
                   (conj! dst (mapv maybe-round position)))))))
    arcs))

(defn sequentialize
  [topo] 
   (assoc topo :arcs (sequentialize-0 (:arcs topo))))

(defn translate
  [topo] 
   (let [arcs (:arcs topo)

         flat-arcs (apply concat arcs)
         firsts    (map first flat-arcs)
         seconds   (map second flat-arcs)

         s-firsts  (sort firsts)
         s-seconds (sort seconds)
         n-points  (count firsts)
         halfway   (quot n-points 2)

         med-1 (if (odd? n-points)
                 (nth s-firsts halfway)
                 (/ (+ (nth s-firsts halfway) 
                       (nth s-firsts (dec halfway)))))
         med-2 (if (odd? n-points)
                 (nth s-seconds halfway)
                 (/ (+ (nth s-seconds halfway) 
                       (nth s-seconds (dec halfway)))))]
     (assoc topo
      :transform {:translate [med-1 med-2]}
      :arcs
          (mapv 
             (fn [arc]
               (mapv
                 (fn [pair]
                   [(- (first pair) med-1) 
                    (- (second pair) med-2)])
                 arc))
             arcs))))

(defn scale
  [topo]
  (let [arcs (:arcs topo)

        flat-arcs (apply concat arcs)
        firsts    (map first flat-arcs)
        seconds   (map second flat-arcs)

        min-firsts (apply min firsts)
        max-firsts (apply max firsts)
        min-seconds (apply min seconds)
        max-seconds (apply max seconds)

        ok-1 (/ (+ min-firsts max-firsts) 2)
        ok-2 (/ (+ min-seconds max-seconds) 2)

        k-1 (if (zero? ok-1) 1 ok-1)
        k-2 (if (zero? ok-2) 1 ok-2)]
    (assoc topo
     :transform {:scale [k-1 k-2] :translate (:translate (:transform topo))}
     :arcs (mapv 
             (fn [arc]
               (mapv
                 (fn [pair]
                   [(maybe-round (/ (first pair)  k-1)) 
                    (maybe-round (/ (second pair) k-2))])
                 arc))
             arcs))))

(defn transform-points-0
  [trans geos] 
  (mapv 
    (fn [geo]
      (if (= (:type geo) "Point")
        (assoc geo :coordinates 
            [(maybe-round (/ (- (first (:coordinates geo)) (first (:translate trans))) (first (:scale trans))))
             (maybe-round (/ (- (second (:coordinates geo)) (second (:translate trans))) (second (:scale trans))))])
        (if (= (:type geo) "MultiPoint")
          (assoc geo :coordinates 
            (mapv 
              (fn [point]
                [(maybe-round (/ (- (first point)  (first (:translate trans))) (first (:scale trans))))
                 (maybe-round (/ (- (second point) (second (:translate trans))) (second (:scale trans))))])
              (:coordinates geo)))
          geo)))
    geos))

(defn transform-points
  [topo] 
   (assoc topo :objects 
    (apply merge
      (map (partial run (partial transform-points-0 (:transform topo)) :geometries) (:objects topo)))))

(defn geom
  [geo] 
  {(keyword (:id geo))
    (no-nil
      {:type "GeometryCollection"
       :id (:id geo)
       :properties (:properties geo)
       :geometries (:features geo)})})

(defn topo
  [ & geos ]
  {:type "Topology"
   :objects 
    (apply merge
      (map geom geos))})

(defn geo2topo
  [ & geos ]
  (-> (apply topo geos)
      (extract-in)
      (cut)
      (extract-out)
      (translate)
      (scale)
      (transform-points)
      (sequentialize)))

(defn write-json
  [dest topo] 
  (spit dest (json/write-str topo)))

