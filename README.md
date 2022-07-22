# DO.utils

<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/allenbaron/DO.utils/branch/main/graph/badge.svg)](https://codecov.io/github/allenbaron/DO.utils?branch=main)
[![R-CMD-check](https://github.com/allenbaron/DO.utils/workflows/R-CMD-check/badge.svg)](https://github.com/allenbaron/DO.utils/actions)
<!-- badges: end -->


`DO.utils` is an R package designed to support the operations of the Human Disease Ontology (DO; [disease-ontology.org](https://disease-ontology.org/)). Currently, the package supports 3 major areas of work:

1. DO Management/Analysis
2. DO Scientometrics
3. Simplifying common R tasks (general utilities)

_It is very much a work in progress._ If you are interested in contributing, feel free to reach out. Please note that our goal is to make functions as broadly useful as possible.


## Installation

```r
# Install DO.utils from GitHub:
# install.packages("devtools")
devtools::install_github("allenbaron/DO.utils")
```


## DO Management/Analysis

`DO.utils` provides the following management/analysis capabilities:

1. Git repo management, iterative execution across git repository tags, and SPARQL queries implemented with wrappers (`DOrepo()`, `owl_xml()`) around `pyDOID` python package.
2. Automation of [disease-ontology.org](https://disease-ontology.org/) updates, including:
    - Statistics plots (see https://disease-ontology.org/about/statistics)
    - Automated html to build the [Users of the Disease Ontology](https://disease-ontology.org/community/collaborators) list
3. Definition source URL validation.
4. Prediction of mappings/cross-references between resources & DO, via [PyOBO/GILDA](https://github.com/pyobo/pyobo) or approximate string matching.
5. Simplified system installation of the OBO tool [ROBOT](http://robot.obolibrary.org/).


## DO Scientometrics

Scientometrics are a measure of the impact of the Disease Ontology on science and `DO.utils` includes measures that are citation-based (bibliometrics) and others which are not.

1. Bibliometrics
    - Collection of publication "cited by" lists from PubMed (via NCBI Entrez Utilities) and Scopus.
    - Citation merging.
2. Direct quantification of DO use by databases/tools, including:
    - DO annotations in the Alliance of Genome Resources model organism databases (MODs).
    - Use metrics for DO-dependent Bioconductor packages (DOSE, DO.db)


## General Utilities

`DO.utils` also includes general utilities to make programming in R easier. The utilities assist with:

- Type/content testing -- `is_blank()`, `is_positive()`, `is_vctr_or_df()`, `all_duplicated()`)
- Vector-to-scalar conversion -- `collapse_to_string()`, `unique_if_invariant()`)
- Data reduction -- `collapse_col()`, `drop_blank()`)
- Value replacement -- `replace_null()`, `replace_blank()`)
- Sorting (by a specified priority)
- Dates -- `cur_yr()`, `today_datestamp()`)
- Temporary workarounds -- `restore_names()`
