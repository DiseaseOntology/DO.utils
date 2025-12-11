# Instantiate an owl_xml object

Instantiate a python `owl.xml` object from the
[pyDOID](https://github.com/allenbaron/pyDOID) package (powered by
[reticulate](https://github.com/rstudio/reticulate)). The file will be
loaded into memory when instantiated. Though named "owl" this method
will also work for RDF files.

## Usage

``` r
owl_xml(path)
```

## Arguments

- path:

  The path to an OWL (RDF) XML-formatted file, as a string.

## Classes & Methods

The `owl.xml` class thinly wraps a small set of python
[rdflib](https://rdflib.readthedocs.io/en/stable/) methods to enable
SPARQL 1.1 queries, with the following methods:

- `owl_xml$query(query, reload)`: Execute a SPARQL 1.1 query. `query`
  may be a string or the path to a .sparql/.rq file. Use `reload = TRUE`
  to force reload a file into memory; useful when iterating through
  releases/tags with `DOrepo$tag_iterate()`.

- `owl_xml$load()`: Manually load a file into memory. Generally, this
  should not be necessary.

## See also

Other pyDOID classes:
[`DOrepo()`](https://allenbaron.github.io/DO.utils/reference/DOrepo.md)
