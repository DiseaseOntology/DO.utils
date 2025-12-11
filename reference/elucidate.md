# Elucidate the Data in an Object

Elucidates (makes clear) what data is in a given object. `elucidate()`
is similar to [`utils::str()`](https://rdrr.io/r/utils/str.html) and
[`dplyr::glimpse()`](https://pillar.r-lib.org/reference/glimpse.html)
but provides more detail on the nature of the data in an object.

## Usage

``` r
elucidate(x, type = "basic", print = TRUE, ...)

# S3 method for class 'omim_inventory'
elucidate(x, type = "basic", print = TRUE, ...)
```

## Arguments

- x:

  An R object with a defined `elucidate()` method.

- type:

  The type of report to create, as a string; either "basic" (default) or
  "full". See description of the Value returned by individual methods
  for more details.

- print:

  Whether to print a basic elucidation to standard out (default:
  `TRUE`).

- ...:

  Additional arguments for methods. Not currently used.

## Value

`omim_inventory` method:

- For `type = "basic"`, a single `tibble` of OMIM and DOID statistical
  information including the total number of terms for each, the number
  of OMIM terms present/absent in DO, the number of deprecated DO terms,
  and the number of terms with one-to-many matches (in either
  direction).

- For `type = "full"`, the "basic" statistical `tibble` and additional
  `tibble`'s containing the full records for the following:

  - `doid_deprecated`,

  - `omim_to_many`: Results where an OMIM ID corresponds to multiple
    DOIDs, excluding skos broad/narrow/related matches.

  - `doid_to_many`): Results where a DOID corresponds to multiple OMIM
    IDs, excluding skos broad/narrow/related matches.
