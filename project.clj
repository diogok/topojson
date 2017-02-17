(defproject topojson "0.0.1"
  :description "TopoJSON for Clojure"
  :url "http://github.com/diogok/topojson"
  :license {:name "MIT"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.json "0.2.6"]
                 [prismatic/plumbing "0.5.3"]
                 [com.rpl/specter "0.13.1"]]
  :global-vars {*warn-on-reflection* true}
  :source-paths ["src"]
  :repositories [["clojars" {:sign-releases false}]]
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[midje "1.7.0"]]
                   :plugins [[lein-midje "3.1.3"]]}})

