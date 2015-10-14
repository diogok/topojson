# topojson

Read and write [TopoJSON](https://github.com/mbostock/topojson/wiki) to and from [GeoJSON](http://geojson.org), in clojure.

## Usage

Include the dependency in your project:

[![Clojars Project](http://clojars.org/topojson/latest-version.svg)](http://clojars.org/topojson)

### Reading 

This convert a TopoJSON structure to a GeoJSON data structure.

```clojure
(use ['topojson.reader :only ('topo2geo 'read-json)])

(def topojson (read-json "example.topo.json"))
(def geojson (topo2geo topojson))
```

### Writing

This converts a GeoJSON into a TopoJSON structure.

```clojure
(use ['topojson.reader :only ('read-json)])
(use ['topojson.writer :only ('geo2topo)])

(def geojson  (read-json "example.geo.json"))
(def topojson (geo2topo geojson))
```

## License

MIT

