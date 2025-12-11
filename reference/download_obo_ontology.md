# Download OBO Foundry Ontology File

Downloads the current version of one or more ontologies from the OBO
Foundry.

## Usage

``` r
download_obo_ontology(ontology_id, dest_dir, on_failure = "warn", ...)
```

## Arguments

- ontology_id:

  A character vector of OBO Foundry ontology identifier(s) (lowercase,
  as found on http://www.obofoundry.org/). For reference, ontology
  identifiers are also provided in
  [obofoundry_metadata](https://allenbaron.github.io/DO.utils/reference/obofoundry_metadata.md)
  within this package.

- dest_dir:

  Path to directory where files will be saved.

- on_failure:

  A string indicating how to handle download failure:

  - "warn" - produce a warning; includes exit codes for debugging

  - "abort" - abort execution

  - "list_failed" - list URLs that failed (*output format differs*, see
    `Value`)

  - "warn-list_failed" - combination of "warn" and "list_failed"

  - "skip" - do nothing

- ...:

  Additional arguments passed on to
  [`utils::download.file()`](https://rdrr.io/r/utils/download.file.html).

## Value

Unless `on_failure` includes "list_failed", the successfully downloaded
`dest_file`(s); otherwise, a 2-vector list where `successful` =
`dest_file`(s) and `failed` = `url`(s).
