# DO.utils (development version)

## Updates
* `collapse_col()` gained all the methods of `collapse_col_flex()`, along with
`na.rm` used by all methods.

## New
* `extract_as_tidygraph()`: Extracts nodes and relationships identified by a
    SPARQL query from RDF/XML file and returns it as a tidygraph.
* `write_graphml()`: Writes a graph object (tidygraph/iGraph) to a .graphml
    file.


# DO.utils 0.2.6

Change license to CC0 1.0 Universal to match standard for the Human Disease
Ontology project and in preparation for use in resource use assessment
publication.


# DO.utils 0.2.5

## Deprecations
**Website**
* `make_user_list_html()` is deprecated because the user/use case information on
    disease-ontology.org was moved from the 'Collaborators' page to the new
    'Use Cases' page. Replaced by `make_use_case_html()`.


## Breaking Changes
**Website**
* `plot_*()` no longer include the datestamp in saved file names.

**Formatters**
* `format_doid()` parameters changed:
    * `allow_bare` renamed to `convert_bare`
    * `validate_input` added to allow invalid input to pass-through without
        modification.

**Cited by / Search**
* `read_pubmed_txt()` now parses IDs (PMID, PMCID, & DOI) from citations and
    returns a data.frame that includes a record number, IDs, and full citations
    instead of a vector of citations.


## Bug Fixes
**Cited by / Search**
* `extract_pmid()` updated to recognize 1- to 8-long PubMed IDs which
    should cover the whole set of actual PMIDs; previously limited to
    recognizing 8-long PMIDs.
* `as_tibble.esummary_list()` fixed error due to reduced data output
    (fewer columns of information) from `pubmed_summary()` caused by API changes.
* `match_citations()` now matches DOIs in a case insensitive manner
    bringing it into compliance with the
    [DOI spec](https://www.doi.org/doi_handbook/2_Numbering.html#2.4).


## Updates
**URLs**
* `append_to_url()`:
    * Gained a new parameter `preserve_NA`, which allows `NA` values to pass
        through instead of being appended to the URL.
    * Added 'github' and 'orcid' as named URLs that might be appended to (via
        `get_url()`).

**Datasets**
* More prefixes in `ns_prefix` and new prefix subsets added:
    * `not_obo_prefix`: Subset of `ns_prefix` with everything except OBO
    ontology prefixes (e.g. 'dc', 'terms', 'skos', 'owl', etc.).
    * `obo_prefix`: Subset of `ns_prefix` with standard OBO ontology prefixes
    and namespaces.
    * `obo_prop_prefix`: New set of prefixes created to represent frequently
    used OBO ontology property prefixes. NOTE: There is one per ontology but
    these may not exist or may not be the actual property prefix used by the
    stated ontology... use with caution.

**General Utilities**
* `unique_to_string()`/`vctr_to_string()`: added `sort` and other arguments for
control of sorting.


## New
**Ontology Extracters/Modifiers**
* `extract_*_axiom()` family: Extract equivalentClass ('eq'), subClassOf
    ('subclass'), or both ('class') logical axioms.
* `queue_xref_split()`: Creates a 'curation queue' of diseases that may need to
    split because they have multiple cross-references from the same source.
* `tidy_sparql()`: Tidies SPARQL query results.

**Website**
* `update_website_count_tables()`: Update counts in tables on 'DO Imports' and
    'DO Slims' pages with data from a specified release of doid-merged.owl.
    _Updates data in place_.
* `make_use_case_html()`: Produces the html for the new 'Use Cases' page, split
    into 3 files, 1 per section for: Ontologies, Resources, and Methodologies.
    * _Does not update data in place._ HTML for the rows & cells must be copied
        and pasted over the HTML for each section in the 'Use Cases' file.
    * Content is sorted alphabetically by column.
* `make_contributor_html()`: Produces HTML list of contributors as `<li>`
    elements for disease-ontology.org, including links to Github and ORCID, as
    available.

**URLs**
* `format_hyperlink()`: Converts URLs into hyperlinks for Google Sheets, Excel,
    or HTML.
* `build_hyperlink()`: Shorthand for common `append_to_url()` plus
    `format_hyperlink()` combination.

**Cited by / Search**
* `pub_id_match` (DATA): A named character vector of regex's to identify/extract
    publication IDs (currently PMID, PMCID, DOI, & Scopus EID).

**General Utilities**
* `sandwich_text()`: Pastes text around strings.
* `wrap_onscreen()`: Wraps messages to be printed on the screen.
* `invert_sublists()`: Swaps the list elements between depths 2 & 3, essentially
    inverting the grouping.
* `lengthen_col()`: Splits column(s) by a delimiter and lengthens the data.frame
    so each value is in its own row.
    * **NOTES:**
        * This is the reverse of `collapse_col()` but will not recreate the
            original data.frame after round trip _in most cases_.
        * Uses `unnest_cross()` internally so the results will always be the
            cartesian product of lengthened columns.
* `count_delim()`: Counts the values in delimited columns; essentially the
    combination of `lengthen_col()` and `dplyr::count()`.

**Type Predicates**
* `is_valid_obo()`: Tests whether the elements of a character vector are 'valid'
    OBO Foundry IDs (based on formatting, not actual existence).

**Formatters**
* `format_obo()`: Formats OBO Foundry IDs.
* `format_axiom()`: Formats OWL functional syntax EQ/SubClassOf axioms to be
    more human readable, similar to that of Protege.


# DO.utils 0.2.4

## New
**Testing**
* `is_boolean()`: T/F type predicate.
* `write_access()`: Test for file write existence with write access.

**Data Conversion**
* `to_curie()` & `to_uri()`: Convert between URI & CURIEs.
* `to_range()`: Convert vector of values to ranges (output as single string).

**Data.frame Manipulation**
* `append_empty_col()`: Add empty columns to a data.frame.
* `unnest_cross()`: Unnest list columns in data.frame _always_ creating the
    cartesian product.
    * Useful for expanding list columns produced by some SPARQL queries.

**Datasets**
* `ST_pubs`: Information about official publications describing the Symptom
    (SYMP) and/or Pathogen Transmission (TRANS) ontologies.
    * Currently, only one conatins one publication.
* `ns_prefix`: Named character vector of common namespace-prefix pairs used in
    DO and/or OBO ontologies.

## Updates
* `cast_to_string` renamed to `collapse_to_string()`
* `DO_pubs` now includes `lens_id` with lens.org identifiers for each publication.

## Dependencies
* Imports 'tidyselect', which is explicitly required for `unnest_cross` but is
    used throughout DO.utils to enable tidyverse-style semantics (via dplyr).


# DO.utils 0.2.3

## Cited By -- Updates
* `citedby_pubmed()`, equivalent to `citedby_scopus()`, now available (uses
    `citedby_pmid()` and `pubmed_summary()` internally.
* `match_citations()` improvments:
    * **BREAKING CHANGE** -- `add_col` argument changed from
    boolean to `NULL` (replaces `FALSE`) or the name of the column (replaces
    `TRUE`).
    * Bug fixes & message improvements.
* `extract_pmid()` has a new argument, `no_result`, which provides control over
    the condition signaled when no results are found (error, warning, message, or
    none); previously a simple error was signaled. For any condition signaled,
    there is now an additional class `no_result` to improve condition handling.
    * For the `elink` method the default for `no_result` is still "error" while
     for `elink_list` the default is now a warning.
* `as_tibble.esummary_list()` bug caused when some results have no data fixed --
    caused errors in `as_tibble.esummary_list_nested()` precipitated by the
    tidyverse's move to more strict vector merging.

**IMPORTANT NOTE:**
The "cited by" functionality of `DO.utils` _may no longer be improved_ because
recent review of Lens.org results suggests it may be a good replacement for this
PubMed + Scopus search & merge strategy.

If improvements are made they will likely facilitate one or more of:

* Merging PubMed and Scopus "cited by" results, probably using `standardize()`
    (or similar).
* Reducing data requested from the APIs by implementing timeframe parameters in
    `citedby_*()`.


# DO.utils 0.2.2

* Bug fix to correct error in `format_subtree()` when the subtree did _not_ have
    any classes with multi-parentage. (Error label: "no fill needed")


# DO.utils 0.2.1

## New
* Create a text-based subtree/hierarchy.
    * `extract_subtree()` extracts the data from doid.owl including all
        descendants and their relationships below a specified DOID.
    * `format_subtree()` arranges them in a dataframe as a text-based hierarchy
        mirroring [disease-ontology.org](https://disease-ontology.org/).
    * Primarily designed for creating high quality "tree view" graphics similar
        to EBI's [Ontology Lookup Service](https://www.ebi.ac.uk/ols/ontologies/doid/terms?iri=http%3A%2F%2Fpurl.obolibrary.org%2Fobo%2FDOID_3070).
* Manage DOIDs.
    * `is_valid_doid()` tests whether inputs are valid DOIDs. Note that mutliple
        formats are considered valid.
    * `format_doid()` converts between valid DOID formats.

## Updates
* `DOrepo()` and `owl_xml()` no longer fail silently when a file/directory does
not exist. pyDOID now verifies file paths on instantiation of the underlying
objects.

## Dependencies
* Suggests tidygraph, which is required for `format_subtree()`.


# DO.utils 0.2.0

## Data
* Added DO Nucleic Acids Research 2022 publication data to `DO_pubs`.

## Dependencies
* R packages:
    * `reticulate` >= v1.23 required.
    * `tidyr` v1.2.0 introduced breaking changes by requiring "safe" type
        casting (implemented via `vctrs`).
        * `replace_na.list()` is now deprecated (requires `tidyr` <= 1.1.4).
* Python dependency: `pyDOID`

## General Purpose
* Created `collapse_col_flex()` to collapse data frame columns more flexibly.
    * Adds two new methods beyond "unique": "first" & "last".
    * Adds the ability to collapse columns using different methods.
* Created wrapper functions for `pyDOID` classes.
    * `DOrepo()` wraps the `pyDOID.repo.DOrepo` class
    * `owl_xml()` wraps the `pyDOID.owl.xml` class

## Graphics / Website
* Updated:
    * `plot_citedby()` to a stacked bar chart showing publication types.
    * `DO_colors` to include saturated versions (names prefixed with `sat_`).
    * Functions generating html have been updated to match html style guide
        standards.
* Created:
    * `theme_DO()` a `ggplot2` plotting theme for DO.
    * `plot_def_src()` to display the number of times a source is used to
        support disease definitions in the ontology (designed for
        disease-ontology.org/about/statistics).

## Cited by
* Renamed:
    * `match_citations_fz()` to `match_fz()`
    * `concat_pm_citation()` to `read_pubmed_txt()`
* Updated `match_citations()` to utilize Scopus EIDs.
* Created:
    * `pmc_summary()`, a parallel to `pubmed_summary()` that works for PubMed
        Central.
    * `hoist_ArticleIds()` (internal) - tidies PubMed/PMC identifiers
        * `tidy_ArticleId_set()` (internal)
    * `as_tibble()`, method `esummary_list_nested`

## URLs
* Created:
    * Functions to read the doid-edit.owl file and extract URLs (+ helpers).
        * `read_doid_edit()`
        * `extract_doid_url()`
    * Functions designed for URL validation.
        * `validate_url()` + helpers
        * **NOTE:** Helpers for robots.txt respectful validation remain
            _INCOMPLETE_ and care should be taken not to overwhelm web servers
            with requests.


# DO.utils 0.1.7

* Setup package to wrap python via `reticulate` package.
* Added functionality to predict mappings using GILDA grounding (a type of
  lexical string matching/natural entity recognition) via python and the python
  modules `pyobo`, `indra`, and `gilda`. New functions:
    * `pyobo_map()` to create the predicted mappings.
    * `parse_mapping()` to parse the python.gilda.ScoredMatch results object to
      a list of data frames with matches (1 df/input term).
    * `unnest_mapping()` to unnest a list column generated by `pyobo_map()`
      inside a `dplyr::mutate()` call; wraps `parse_maping()`.


# DO.utils 0.1.6

## General
* Added **DEPENDENCIES** on `ggplot2`, `googlesheets4`, and `glue`.
* Renamed `match_citations_fz()` to `match_fz()`.
* Added `cast_to_string()`, a more generalized version of `vctr_to_string()`
    that accepts multiple inputs (similar to `paste()`).
* Added function to `partition()` vectors into groups with `n` elements per
    group.

## Data
* Added latest official DO publication to `DO_pubs`.
* Added official `DO_colors`.

## Feature: Website Updates
* Added functions to create statistics graphs: `plot_citedby()`,
    `plot_terms_def_counts()`, `plot_branch_counts()`, `plot_xref_counts()`.
* Added `make_user_list_html()` to create rows of table in Community >
    Collaborators > Users of the Disease Ontology from the DO team's curated
    "Uses" Google sheet.

## Internal Use Only
* Added `to_character()`, helper for `cast_to_string()`, to reduce lists and
    data frames to character vectors while limiting data loss.
* Added `html_in_rows()`, helper for `make_user_list_html()`, to format html
    elements in rows (with optional row & cell attributes).
* Added Google sheets identifiers for programmatic access.



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
