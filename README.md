# topojson

Read and Write TopoJSON in clojure.

## Usage

### Reading

```clojure
(use ['topojson.reader :only ('topo2geo 'read-json)])

(def topojson (topo2geo (read-json "example.topo.json")))
```

### Writing

TODO

## License

MIT

