(ns topojson.writer)

(defn extract
  [geo] nil)

(defn join
  [geo] nil)

(defn cut
  [geo] nil)

(defn dedup
  [geo] nil)

(defn scale
  [geo] nil)

(defn translate
  [geo] nil)

(defn transform
  [geo] (-> geo (translate) (scale)))

(defn geo2topo
  [geo]
   (-> geo
       (extract)
       (join)
       (cut)
       (dedup)
       (transform)))



