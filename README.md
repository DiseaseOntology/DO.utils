# DO.utils

This R package provides a variety of functions used to support the operations of the Human Disease Ontology (DO; <disease-ontology.org>).

Currently, the package provides for:

1. DO Scientometrics
2. Simplifying common R tasks (basic utilities)

_It is very much a work in progress._ If you are interested in contributing, feel free to reach out.


## Installation

```r
# Install DO.utils from GitHub:
# install.packages("devtools")
devtools::install_github("allenbaron/DO.utils")
```


## DO Scientometrics

Here scientometrics focuses on measuring the impact of DO on science. This includes measures that are citation-based (bibliometrics) and those that are not.

The scientometric capabilities included in this package are:

- Bibliometrics
    - NCBI Entrez Utilities: `tidy_pubmed_summary()`
- Quantifying DO annotations in the Alliance of Genome Resources model organism databases (MODs)
    - `download_AGR()`, `read_AGR()`, `count_AGR_records()`
- Quantifying metrics for DO-dependent Bioconductor packages (DOSE, DO.db) _[INCOMPLETE]_
    - `get_bioc_pkg_stats()`


## Basic Utilities

The basic utilities included in this package simplify the following in R:

- Dates: `cur_yr()`, `today_datestamp()`
- Type Predicates
    - character: `is_blank()`, `is_missing()`
    - numeric: `is_positive()`, `is_negative()`
- Vector to scalar conversion: `vctr_to_string()`, `unique_if_invariant()`
- Temporary workarounds: `restore_names()`
