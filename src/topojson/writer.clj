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

(defn indexed-arcs-0
  [arcs] 
  (println "index")
  (let [index (time (apply hash-map (interleave arcs (map set arcs))))]
    (time
      (apply merge
        (map 
          (fn [arc]
            {arc (apply union (vals (dissoc index arc)) )})
        arcs)
      )
      )))

(defn indexed-arcs-1
  [arcs] 
  (apply hash-map (interleave arcs (map set arcs))))

(defn indexed-arcs
  [arcs] 
  (apply hash-map (interleave arcs (map (fn [arc] (apply hash-map (interleave arc arc))) arcs))))

(defn cut-arc-1
  [index arc]
  (println "cut-arc" (count arc))
  (let [arcs (vals (dissoc index arc))]
    (time
    (doall
    (partition-by
      (fn [pair]
        (loop [arcs arcs]
          (if (empty? arcs) nil
            (if ((first arcs) pair)
              pair
              (recur (rest arcs))
              ))))
      arc)))))

(defn cut-arc-0
  [index arc]
  (println "cut-arc" (count arc))
  (let [arcs  (vals (dissoc index arc))
        pairs (set arc)
        inter (time (intersection pairs arcs))]
    (time
    (doall
    (partition-by
      inter
      arc)))))


(defn cut-arc
  [index arc]
  (println "cut-arc" (count arc))
  (let [arcs  (vals (dissoc index arc))]
    (time
    (doall
    (partition-by
      (fn [pair] (some (fn [arc] (arc pair)) arcs))
      arc)))))

(defn cut-at-junctions
  [arcs] 
  (println "cut-at-junctions" (count arcs))
  (let [index (time (indexed-arcs arcs))]
    (time
    (doall
      (distinct
        (mapcat
          (partial cut-arc index)
          arcs))))))

(defn translate
  [arcs] 
  (println "translate")
   (let [flat-arcs (time (doall (apply concat arcs)))
         firsts    (time (doall (map first flat-arcs)) )
         seconds   (time (doall (map second flat-arcs)) )

         s-firsts  (time (sort firsts) )
         s-seconds (time (sort seconds) )
         n-points  (time (count firsts) )
         halfway   (time (quot n-points 2) )

         med-1 (time (if (odd? n-points)
                 (nth s-firsts halfway)
                 (/ (+ (nth s-firsts halfway) 
                       (nth s-firsts (dec halfway)))))
 )
         med-2 (time (if (odd? n-points)
                 (nth s-seconds halfway)
                 (/ (+ (nth s-seconds halfway) 
                       (nth s-seconds (dec halfway)))))
 )         
         ]
     {:translate [med-1 med-2] 
      :arcs
      (time
      (mapv 
             (fn [arc]
               (mapv
                 (fn [pair]
                   [(- (first pair) med-1) 
                    (- (second pair) med-2)])
                 arc))
             arcs)
      )
      }))

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

(defn get-arcs-0
  [arcs line]
  (println "get-arcs-0")
  (let [result (transient [])]
    (doseq [start (range 0 (count line))
            limit (reverse (range 0 (inc (count line))))]
      (let [line (->> line (drop start) (take (- limit start)))]
        (if (not (empty? line))
          (if-let [arc (some #{line} arcs)]
            (conj! result (.indexOf arcs arc)))
          )))
    (persistent! result)))

(defn get-arcs
  [arcs geo]
  (condp = (:type geo)
    "Polygon"
      (mapv (partial get-arcs-0 arcs) (:coordinates geo))
    "MultiPolygon"
      (mapv
        (fn [poly]
          (mapv (partial get-arcs-0 arcs) poly))
        (:coordinates geo))
    "LineString"
      (get-arcs-0 arcs (:coordinates geo))
    "MultiLineString"
    (mapv
      (partial get-arcs-0 arcs)
      (:coordinates geo))
    nil))

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

