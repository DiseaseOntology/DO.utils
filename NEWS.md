# DO.utils (development version)

* Rename `match_citations_fz()` to `match_fz()`


# DO.utils 0.1.5

## General
* Added a `NEWS.md` file to track changes to the package.
* Created/updated various **helpers to manage data** (get it, make it easier to
  save/track with version control):
    * `download_file()` to flexibly handle multiple downloads.
        * Created `download_status` Ref Class to manage downloads based on exit
          code/status.
    * `confine_list()` / `release_list()` to reversibly convert a list column to a
      character vector (using json).
    * `is_invariant()` to test for vectors with only 1 value; with methods for
      character & numeric vectors.
    * `unique_to_string()` to collapse vectors to strings.
    * `unique_if_invariant()` to _conditionally_ collapse vectors with only 1
      value.
    * Added `na.rm` argument to all vctr_to_scalar functions.
    * `replace_*()`
        * `NA`, method for lists.
        * `NULL`, to replace NULL values in lists recursively.
        * `blank`, to replace "" values.
    * `collapse_col()` to collapse 1 or more specified columns in a data frame
      by concatenating unique values together, while preserving unique values in         all other columns.
* Created `download_obo_ontology()` to download 1 or more ontologies maintained
  by the OBO Foundry.

## Data
* Added more to DO publication info.
* Added OBO Foundry metadata.

## Feature: Alliance
* Made it possible to count Alliance terms for a subset of DOIDs.
* Increased record type count options (arg: record_lvl) --> "full_record",
"disease-object", "disease", "object"

## Feature: citedby
* **Created `citedby_scopus()`** to get cited by publication data from Scopus
  API, along with s3 classes & methods to manage Scopus data.
    * Updated to capture datetime citedby data is first retrieved.
* Renamed `citedby_pubmed()` to `citedby_pmid()` to reflect its output.
    * **NEED** new `citedby_pubmed()` that combines `citedby_pmid()` &
      `pubmed_summary()`.
* Modified `pubmed_summary()` to accept PMID lists as input.
    * Uses new `extract_pmid()` `elink_list` method.
* Changed citedby `tidy()` methods to `as_tibble()` methods & added new methods.
* Created `truncate_authors()` to shorten long PubMed author lists.
* Created `get_url()` & `append_to_url()` to build DOI, PubMed, and PMC URLs for
  individual publications.
