(defproject topojson "0.0.3"
  :description "TopoJSON for Clojure"
  :url "http://github.com/diogok/topojson"
  :license {:name "MIT"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/data.json "0.2.6"]
                 [prismatic/plumbing "0.5.5"]
                 [com.rpl/specter "1.1.0"]]
  :global-vars {*warn-on-reflection* true}
  :source-paths ["src"]
  :repositories [["clojars" {:sign-releases false}]]
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[midje "1.9.1"]]
                   :plugins [[lein-midje "3.2.1"]]}})

