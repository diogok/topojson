(ns topojson.writer
  (:require [topojson.reader :refer [maybe-round no-nil]])
  (:require [clojure.set :refer :all]))

(defn extract-lines
  [geo] 
  (condp = (:type geo)
    "Point" []
    "MultiPoint" []
    "LineString" 
      [(:coordinates geo)]
    "MultiLineString" 
      (:coordinates geo)
    "Polygon" 
      (:coordinates geo)
    "MultiPolygon" 
      (into [] (apply concat (:coordinates geo)))
    nil))

(defn extract-arcs
  [geo] 
   (condp = (:type geo)
     "Feature"
      (distinct (extract-lines (:geometry geo)))
     "FeatureCollection"
      (into []
        (distinct
          (apply concat
            (filter #(not (empty? %))
              (mapv extract-arcs (:features geo))))))
     nil))

(defn junctions
  [arcs] 
  (loop [arc  (set (first arcs)) 
         arcs (doall (map set (rest arcs)))
         junctions (transient [])]
    (if (not (nil? arc)) 
      (do
        (doseq [pair arc
                arc arcs]
          (if (arc pair)
            (conj! junctions pair)))
        (recur 
          (first arcs)
          (rest arcs)
          junctions))
      (set (persistent! junctions)))))

(defn cut-at-junctions
 [arcs] 
  (println "cut at junctions")
  (let [junctions (junctions arcs)]
   (loop [arc (first arcs)
          arcs (rest arcs)
          final-arcs (transient [])]
     (if (not (nil? arc))
       (do
         (doseq [part (partition-by junctions arc)]
           (conj! final-arcs part))
         (recur 
           (first arcs)
           (rest arcs)
           final-arcs))
       (distinct (persistent! final-arcs))))))

(defn translate
  [arcs] 
  (println "translate")
   (let [flat-arcs (doall (apply concat arcs))
         firsts    (doall (map first flat-arcs))
         seconds   (doall (map second flat-arcs)) 

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
     {:translate [med-1 med-2] 
      :arcs
      (mapv 
             (fn [arc]
               (mapv
                 (fn [pair]
                   [(- (first pair) med-1) 
                    (- (second pair) med-2)])
                 arc))
             arcs)}))

(defn scale
  [arcs]
  (println "scale")
  (let [flat-arcs (apply concat arcs)
        firsts    (map first flat-arcs)
        seconds   (map second flat-arcs)

        min-firsts (apply min firsts)
        max-firsts (apply max firsts)
        min-seconds (apply min seconds)
        max-seconds (apply max seconds)

        ok-1 (/ (+ min-firsts max-firsts) 2)
        ok-2 (/ (+ min-seconds max-seconds) 2)

        k-1 (if (zero? ok-1) 1 ok-1)
        k-2 (if (zero? ok-2) 1 ok-2)
        ]
    {:scale [k-1 k-2]
     :arcs (mapv 
             (fn [arc]
               (mapv
                 (fn [pair]
                   [(maybe-round (/ (first pair)  k-1)) 
                    (maybe-round (/ (second pair) k-2))])
                 arc))
             arcs)}))

(defn transform-0
  [arcs]
  (println "transform")
  (let [translated (translate arcs)
        scaled   (scale (:arcs translated))]
    {:transform {:scale (:scale scaled)
                 :translate (:translate translated)}
     :arcs (:arcs scaled)}))

(defn transform
  [topo] 
  (let [transformed (transform-0 (:arcs topo))]
    (assoc topo
      :arcs (:arcs transformed)
      :transform (:transform transformed))))

(defn sequentialize-0
  [arcs] 
  (println "seqit")
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

(defn invert
  [arcs] 
  (loop [i 0 total (count arcs) inverted (transient {})]
    (if (= i total) (persistent! inverted)
      (recur 
        (inc i)
        total
        (assoc! inverted (arcs i) i))
      )
    )
  )

(defn get-arcs-0
  [inverted-arcs line]
  (println "get-arcs-0")
  (time
  (let [result   (transient [])]
    (doseq [start (range 0 (count line))
            limit (reverse (range 0 (inc (count line))))]
      (let [line (->> line (drop start) (take (- limit start)))]
        (if (not (empty? line))
          (if-let [arc-n (inverted-arcs line)]
            (conj! result arc-n))
          ))
          )
    (persistent! result)))
  )

(defn get-arcs
  [arcs geo]
  (let [get-arcs-1 (partial get-arcs-0 (invert (vec arcs )))]
  (condp = (:type geo)
    "Polygon"
      (mapv get-arcs-1 (:coordinates geo))
    "MultiPolygon"
      (mapv
        (fn [poly]
          (mapv get-arcs-1 poly))
        (:coordinates geo))
    "LineString"
      (get-arcs-1 (:coordinates geo))
    "MultiLineString"
    (mapv
      get-arcs-1
      (:coordinates geo))
    nil)))

(defn convert
  [transform arcs geo] 
   (condp = (:type geo)
     "Feature"
        (no-nil
          {:type (:type (:geometry geo))
           :id (:id geo)
           :properties (:properties geo)
           :coordinates (if (= "Point" (:type (:geometry geo))) 
                          (let [n1 (first (:coordinates (:geometry geo)))
                                n2 (second (:coordinates (:geometry geo)))]
                              [(/ (- n1 (first (:translate transform)))
                                   (first (:scale transform)))
                               (/ (- n2 (second (:translate transform))) 
                                   (second (:scale transform)))])
                          nil)
           :arcs (if (not (= "Point" (:type (:geometry geo))))
                   (get-arcs arcs (:geometry geo)) nil)
            })
     "FeatureCollection"
        (no-nil
          {:type "GeometryCollection"
           :id (:id geo)
           :geometries (mapv (partial convert transform arcs) (:features geo))})
     nil))

(defn as-topo
  [arcs geos] 
  (let [transformed (transform-0 arcs)]
    {:type "Topology"
     :transform (:transform transformed)
     :arcs (sequentialize-0 (:arcs transformed))
     :objects 
       (apply merge
        (map 
         (fn [geo]
           {(keyword (or (:id geo) (apply str "hashed" (hash geo))) )
            (convert (:transform transformed) arcs geo)})
          geos))}))

(defn geo2topo
  [ & geos ]
  (println "GEO2TOPO")
   (let [single-geo {:type "FeatureCollection" :features (apply concat (map :features geos))}]
     (-> single-geo
         (extract-arcs)
         (cut-at-junctions)
         (as-topo geos))))

