# topojson

Read and Write TopoJSON to and from GeoJSON, in clojure.

## Usage

### Reading 

This convert an TopoJSON to a GeoJSON.

```clojure
(use ['topojson.reader :only ('topo2geo 'read-json)])

(def geojson (topo2geo (read-json "example.topo.json")))
```

### Writing

This convert from a GeoJSON to a TopoJSON.

```clojure
(use ['topojson.reader :only ('read-json)])
(use ['topojson.writer :only ('geo2topo)])

(def topojson (geo2topo (read-json "example.geo.json")))
```

TODO

## License

MIT

