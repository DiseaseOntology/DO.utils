# Extract OWL/RDF as a tidygraph

Extract 'nodes' and their 'parents' defined by a SPARQL `query` from
OWL/RDF as a tidygraph.

## Usage

``` r
extract_as_tidygraph(x, query = NULL, collapse_method = "first", debug = FALSE)
```

## Arguments

- x:

  A 'pyDOID.owl.xml' object or the path to an OWL/RDF XML file that can
  be instantiated as such an object by
  [`owl_xml()`](https://allenbaron.github.io/DO.utils/reference/owl_xml.md).

- query:

  A SPARQL 1.1 query, as a string or the path to a .sparql/.rq file. If
  `NULL`, input is assumed to be an OBO Foundry ontology and the
  `oboInOwl:id` and `rdfs:label` of all OBO classes will be extracted.
  See "Query Requirements" section for more information.

- collapse_method:

  The method to use when collapsing extra data columns, as a string. See
  "Query Requirements" section for more information.

- debug:

  *\[Intended for developers only\]* Whether to debug execution. Returns
  all intermediate objects as part of output.

## Query Requirements

[tidygraph](https://tidygraph.data-imaginist.com/reference/tidygraph-package.html)
expects *unique* child-parent relationships, so at a minimum the SPARQL
`query` should include `?id` (some identifier for a 'child') and
`?parent` (some identifier for each child's parent(s)). All additional
output variables specified in the SPARQL query will be treated as 'node'
annotations and collapsed using a method from
[`collapse_col_flex()`](https://allenbaron.github.io/DO.utils/reference/collapse_col_flex.md)
to prevent duplication of records.
