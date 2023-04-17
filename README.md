# DO.utils

<!-- badges: start -->
[![R-CMD-check](https://github.com/DiseaseOntology/DO.utils/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/DiseaseOntology/DO.utils/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/DiseaseOntology/DO.utils/branch/main/graph/badge.svg)](https://app.codecov.io/gh/DiseaseOntology/DO.utils?branch=main)
<!-- badges: end -->


DO.utils is an R package _primarily_ designed to support the operations of the Human Disease Ontology (DO; [disease-ontology.org](https://disease-ontology.org/)) but with **a number of capabilities that will be useful to the broader scientific community** for:

1. **Assessing Resource Use & Impact** (Bibliometrics/Scientometrics)
    - A brief summary can be found under [Assessing Resource Use (Bibliometrics/Scientometrics)](#assess_use).
    - For a detailed description refer to the "Assessing Resource Use: Obtaining Use Records" tutorial included with this package (`vignette("obtain_use_records", package = "DO.utils")`) or the peer-reviewed article:
    
    > J. Allen Baron, Lynn M Schriml, _Assessing resource use: a case study with the Human Disease Ontology_, Database, Volume 2023, 2023, baad007. PMID:36856688, https://doi.org/10.1093/database/baad007.
    
2. Simplifying common R tasks (see [General Utilities](#general) section).

Operations specific to the use, analysis, maintenance, and improvement of the ontology itself are described briefly in the [DO Improvement & Analysis](#do_specific) section.

DO.utils is _work in progress_. If you are interested in contributing, please reach out. Note that our goal is to work collaboratively to make functions as broadly useful as possible.


## Installation

### Installing Prerequisites

To use DO.utils you must first install `R` from [CRAN](https://cran.r-project.org/).
Installing [RStudio]() can also be useful but is not required. The `devtools` package is also required and can be obtain by executing `install.packages("devtools")` within R.

### Installing DO.utils

DO.utils can be installed from Github or from a persistent, open-access repository hosted by Zenodo.

To install from Github, run `devtools::install_github("DiseaseOntology/DO.utils")` within R.

To install from Zenodo, first download DO.utils (DOI: [10.5281/zenodo.7467668](https://www.doi.org/10.5281/zenodo.7467668)) to your local machine. Then, within R run `devtools::install_git(<local_path_to_DO.utils>)`, replacing `<local_path_to_DO.utils>` with the local path to DO.utils.


## Assessing Resource Use & Impact (Bibliometrics/Scientometrics) {#assess_use}

DO.utils includes functions to assist in both assessing how a resource is used and in measuring the impact of that use. Most of these functions may be broadly useful to anyone trying to accomplish these tasks, while a much smaller number are specific to measuring the DO's impact.

Components that will be broadly useful to any resource can:

1. Identify scientific publications that use a resource from:
    1. Citations of one or more article(s) published by the resource ("cited by"; `citedby_pubmed()` and `citedby_scopus()`).
    2. PubMed or PubMed Central (PMC) search results (`search_pubmed()` and `search_pmc()`).
    3. A MyNCBI collection (`read_pubmed_txt()`).
2. Identify matching publication records in different record sets (must be formatted data.frames; see `match_citations()`).

To those interested in Bioconductor package download statistics,`get_bioc_pkg_stats()` may be useful, while other measures of impact are designed specifically with the DO in mind (e.g. `count_alliance_records()`).


## DO Improvement & Analysis {#do_specific}

DO.utils provides the following capabilities used for improvement and analysis:

1. Git repo management, iterative execution across git repository tags, and SPARQL queries implemented with wrappers (`DOrepo()`, `owl_xml()`) around the related [pyDOID](https://pypi.org/project/pyDOID/) python package.
2. Automation of [disease-ontology.org](https://disease-ontology.org/) updates, including:
    - Statistics plots (see https://disease-ontology.org/about/statistics)
    - Semi-automated html rendering to build various pages including the [Use Cases](https://disease-ontology.org/community/use-cases), [DO Imports](https://disease-ontology.org/resources/DO_Imports), [DO Slims](https://disease-ontology.org/resources/DO_Slims), and the new [Symptom Ontology](https://disease-ontology.org/resources/symptom-ontology) and [Pathogen Transmission Ontology](https://disease-ontology.org/resources/pathogen-transmission-ontology) pages.
3. Definition source URL validation.
4. Prediction of mappings/cross-references between other resources & DO, via [PyOBO/GILDA](https://github.com/pyobo/pyobo) or approximate string matching.
5. Simplified system installation of the OBO tool [ROBOT](http://robot.obolibrary.org/).


## General Utilities {#general}

DO.utils includes general utilities to make programming in R easier including, for example, those that assist with:

- Type/content testing -- `is_blank()`, `is_positive()`, `is_vctr_or_df()`, `all_duplicated()`
- Vector-to-scalar conversion -- `collapse_to_string()`, `unique_if_invariant()`
- Data reduction -- `collapse_col()`, `drop_blank()`
- Value replacement -- `replace_null()`, `replace_blank()`
- Sorting (by a specified priority)
- Dates -- `cur_yr()`, `today_datestamp()`
- Temporary bug workarounds -- `restore_names()`
