# Get Bioconductor Package Statistics

Get download statistics for specific Bioconductor packages.

## Usage

``` r
get_bioc_pkg_stats(pkg, pkg_type, yr, delay_rng)
```

## Arguments

- pkg:

  character vector; name of Bioconductor package(s)

- pkg_type:

  character vector of same length as `pkg` identifying the type of
  Bioconductor package; one of "software", "annotation", "experiment",
  or "workflow"

- yr:

  character scalar; 4-digit year of stats desired, default = current
  year (currently implemented only for single yr)

- delay_rng:

  `c(min, max)` number of seconds to wait between requests; default =
  `c(1, 10)`
