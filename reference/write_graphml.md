# Write a Graph to .graphml File

Writes a graph object (tidygraph/iGraph) to a file as
[GraphML](https://en.wikipedia.org/wiki/GraphML).

## Usage

``` r
write_graphml(graph, file)
```

## Arguments

- graph:

  A
  [tidygraph](https://tidygraph.data-imaginist.com/reference/tidygraph-package.html)
  or igraph object.

- file:

  The file path to write to, as a string. If '.graphml' extension is
  missing, it will be appended.

## Value

Absolute path of .graphml file written.
