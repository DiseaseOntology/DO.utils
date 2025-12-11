# Changelog

## DO.utils 0.3.3

### Updates

- [`plot_xref_counts()`](https://allenbaron.github.io/DO.utils/reference/plot_xref_counts.md)
  and
  [`plot_branch_counts()`](https://allenbaron.github.io/DO.utils/reference/plot_branch_counts.md)
  now use ROBOT instead of pyDOID for data extraction.
- [`append_to_url()`](https://allenbaron.github.io/DO.utils/reference/append_to_url.md)
  updated with new PMC URL.

### New

- [`suggest_regex()`](https://allenbaron.github.io/DO.utils/reference/suggest_regex.md)
  takes a list of strings and suggests a regex pattern that will match
  them all.

## DO.utils 0.3.2

### General

### Updates

- [`is_invariant()`](https://allenbaron.github.io/DO.utils/reference/is_invariant.md)
  now works for more than just character & numeric vectors, with new
  `list` and `data.frame` methods and a `default` method that should be
  able to handle more cases (and replaces the `character` method).
- [`sandwich_text()`](https://allenbaron.github.io/DO.utils/reference/sandwich_text.md)
  revised with new `add_dup` argument to control whether placeholders
  are added when they already exist at the start and end of strings.

### DO Management & Analysis

#### BREAKING CHANGES

- [`plot_citedby()`](https://allenbaron.github.io/DO.utils/reference/plot_citedby.md)
  argument `color_set` now requires names and one color for each of the
  7 possible publication types when specifying colors manually.
  - *NEW* `retracted` argument added to specify how retracted articles
    should be managed.
- [`tidy_sparql()`](https://allenbaron.github.io/DO.utils/reference/tidy_sparql.md)
  arguments `as_curies` and `lgl_NA_false` replaced with new `tidy_what`
  argument and the optional tidy procedures has increased to include
  cleaning headers, unnesting list columns, converting URIs to CURIEs,
  outputting data as a tibble, replacing logical missing values with
  FALSE, and remove invariant language tags.
- [`robot_query()`](https://allenbaron.github.io/DO.utils/reference/robot_query.md)
  completely revised to:
  1.  use
      [`DO.utils::robot()`](https://allenbaron.github.io/DO.utils/reference/robot.md)
      instead of a direct system call to a system-wide robot, to improve
      consistency.
  2.  replace `rq` argument with `query`and now accepts query text
      directly, in addition to file paths. It also automatically handles
      different SPARQL query types (i.e. `SELECT`, `UPDATE`,
      `CONSTRUCT`, etc.
  3.  replace `save` argument with `output` and now outputs results to R
      console by default when possible, with the option to save them to
      a file when given a file path.
  4.  allow for additional arguments to passed to ROBOT with `...`.
  5.  *NEW* `tidy_what` argument to use
      [`tidy_sparql()`](https://allenbaron.github.io/DO.utils/reference/tidy_sparql.md)
      on results with the need for a separate function call.
  6.  *NEW* `col_types` to specify column types of results.

#### Updates

- [`read_omim()`](https://allenbaron.github.io/DO.utils/reference/read_omim.md)
  and
  [`inventory_omim()`](https://allenbaron.github.io/DO.utils/reference/inventory_omim.md)
  now use the preferred “MIM:” prefix in their output instead of
  “OMIM:”.
  [`inventory_omim()`](https://allenbaron.github.io/DO.utils/reference/inventory_omim.md)
  has been modified to accept input with either prefix. *This coincides
  with changes in the Human Disease Ontology (see
  <https://github.com/DiseaseOntology/HumanDiseaseOntology/issues/1323>).*
- [`read_omim()`](https://allenbaron.github.io/DO.utils/reference/read_omim.md)
  now additionally:
  1.  parses official API-key requiring phenotypicSeries.txt downloads
      and may be able to handle additional API-key requiring downloads.
  2.  recognizes X-linked inheritance from OMIM.
- [`collapse_col()`](https://allenbaron.github.io/DO.utils/reference/collapse_col.md)
  updated to support negative selection and tidyselect selectors with
  dplyr versions \>= 1.0, which broke these approaches.

#### New

- [`download_omim()`](https://allenbaron.github.io/DO.utils/reference/download_omim.md)
  downloads official API-key requiring files directly from OMIM
  (e.g. mim2gene.txt, phenotypicSeries.txt, etc.).
- [`extract_ordo_mappings()`](https://allenbaron.github.io/DO.utils/reference/extract_ordo_mappings.md)
  extracts mappings from Orphanet Rare Disease Ontology, in native
  format as `oboInOwl:hasDbXref` with Orphanet’s text-based predicate
  modifiers, or as SKOS (supplemented with filler `doid:` predicates
  where `SKOS` predicates don’t exist.

## DO.utils 0.3.1

### Dependency Update

- dplyr \> v1.1.0 now required.

### General

#### New

- [`elucidate()`](https://allenbaron.github.io/DO.utils/reference/elucidate.md)
  (generic) describes the data in a given object. Currently, only
  `omim_inventory` has a defined method.

### DO Management & Analysis

#### Updated

- [`read_omim()`](https://allenbaron.github.io/DO.utils/reference/read_omim.md)
  now additionally parses official OMIM downloads of search results and
  [phenotypic series
  titles](https://www.omim.org/phenotypicSeriesTitles/all).
  - Includes `omim_official` attribute to indicate if the source was an
    official download.
  - If input is an official source, the output class will indicate the
    type.
  - `keep_mim` arg can be used to filter OMIM search results.
- [`tidy_sparql()`](https://allenbaron.github.io/DO.utils/reference/tidy_sparql.md)
  now removes `?` from column names and has the new argument
  `lgl_NA_false` for specifying whether `NA` values should be replaced
  with `FALSE` in logical columns.
- [`write_gs.omim_inventory()`](https://allenbaron.github.io/DO.utils/reference/write_gs.md)
  now has a `datestamp` method.

## DO.utils 0.3.0

### General

#### Updated

- Aligned
  [`read_delim_auto()`](https://allenbaron.github.io/DO.utils/reference/read_delim_auto.md)
  more closely with
  [`readr::read_delim()`](https://readr.tidyverse.org/reference/read_delim.html)
  so it could handle compressed input.
- Broadened
  [`unique_if_invariant()`](https://allenbaron.github.io/DO.utils/reference/unique_if_invariant.md)
  no longer uses it’s own methods and instead relies on
  [`base::unique()`](https://rdrr.io/r/base/unique.html). This may have
  some unintended consequences, particularly where custom methods of
  [`unique()`](https://rdrr.io/r/base/unique.html) are defined but it
  works for more inputs, better matching expectations.
- `format_html()` `txt` argument has been renamed to `text` to align it
  more fully with expectations.

#### New

- [`lexiclean()`](https://allenbaron.github.io/DO.utils/reference/lexiclean.md)
  processes text for improved text matching.
- [`round_zero()`](https://allenbaron.github.io/DO.utils/reference/round_zero.md)
  to round numbers toward zero.
- [`round_down()`](https://allenbaron.github.io/DO.utils/reference/round_down.md)
  to round numbers down; more flexible than
  [`base::floor()`](https://rdrr.io/r/base/Round.html).

### DO Management & Analysis

#### WEBSITE-Supporting Updates

- Fix
  [`make_use_case_html()`](https://allenbaron.github.io/DO.utils/reference/make_use_case_html.md)
  to have case-insensitive sorting.
- [`plot_citedby()`](https://allenbaron.github.io/DO.utils/reference/plot_citedby.md)
  - Now accepts manually-defined color sets, in addition to color sets
    provided by `DO.utils`.
  - Default plot size changed to better fit new position on
    disease-ontology.org statistics page.

#### Other Updates

- [`robot()`](https://allenbaron.github.io/DO.utils/reference/robot.md)
  - `.path` argument renamed to `.robot_path` to avoid use in other
    functions without changing the name.
  - Now informs when testing and caching a ROBOT executable for future
    use.
- [`onto_missing()`](https://allenbaron.github.io/DO.utils/reference/onto_missing.md)
  was poorly designed and has been ***deprecated***. To determine which
  OMIM entries are present in the DO as mappings, use
  [`inventory_omim()`](https://allenbaron.github.io/DO.utils/reference/inventory_omim.md)
  instead.

#### New

- ***NEW FUNCTIONALITY** to speed up curation of OMIM mappings!!!*
  - [`read_omim()`](https://allenbaron.github.io/DO.utils/reference/read_omim.md)
    reads data copied or downloaded from OMIM.
    - Previously an internal function with limited capability to handle
      specific copy/paste operation from OMIM.
    - Now expanded to read data downloaded from omim.org phenotypic
      series pages using the “Download as” button and to handle all copy
      paste of tabular data from omim.org without the need for manual
      corrections.
    - No longer returns `tidy_label` and `provisional` columns, as these
      were not particularly useful, and instead includes `omim` and
      `geno_inheritance` columns to help with curation.
  - [`inventory_omim()`](https://allenbaron.github.io/DO.utils/reference/inventory_omim.md)
    compares OMIM entry records against mappings in the DO and reports
    whether they exist, with accompanying DO class data when they do.
  - [`write_gs()`](https://allenbaron.github.io/DO.utils/reference/write_gs.md)
    (generic) writes data from DO.utils created classes to Google
    Sheets.
    - `omim_inventory` is the first method.
- [`read_ga()`](https://allenbaron.github.io/DO.utils/reference/read_ga.md)
  to read Google Analytics data exported to .csv file.
  - Eliminates the need for time-consuming, corrections to get a GA
    exported file into a tidy format for further use
  - Can optionally read multiple tables from a single file (most exports
    have two).
  - Can *NOT* merge GA data that has been split over multiple files due
    to size. These must be merged manually but this should be trivial.

## DO.utils 0.2.10

### DO Management & Analysis

- [`onto_missing()`](https://allenbaron.github.io/DO.utils/reference/onto_missing.md)
  &
  [`tidy_sparql()`](https://allenbaron.github.io/DO.utils/reference/tidy_sparql.md)
  output now has improved column types (no longer all character
  vectors).
- [`append_to_url()`](https://allenbaron.github.io/DO.utils/reference/append_to_url.md)
  has a new named URL option, “DO_website”, for direct link to disease
  info on disease-ontology.org.

### Assessing Resource Use

- [`citedby_scopus()`](https://allenbaron.github.io/DO.utils/reference/citedby_scopus.md)
  will no longer retain responses with zero results and gained a new
  argument `no_results` to control how these are signaled to the user,
  making it more consistent with
  [`citedby_pubmed()`](https://allenbaron.github.io/DO.utils/reference/citedby_pubmed.md).

## DO.utils 0.2.9

- Fix dplyr code error in
  [`onto_missing()`](https://allenbaron.github.io/DO.utils/reference/onto_missing.md).

## DO.utils 0.2.8

### Highlights

- Adds
  [`onto_missing()`](https://allenbaron.github.io/DO.utils/reference/onto_missing.md)
  and character length-sorting functions.
- More support for creating links from CURIEs.
- Includes fixes to eliminate warnings from use of tidyverse (#15) and
  errors due to updates in some tidyverse packages.

### General Utilities

#### New

- [`length_sort()`](https://allenbaron.github.io/DO.utils/reference/length_sort.md):
  Sorts vector elements by character length.
- [`length_order()`](https://allenbaron.github.io/DO.utils/reference/length_sort.md):
  Sorts data.frames by character length of elements in specified
  column(s).
- [`iff_all_vals()`](https://allenbaron.github.io/DO.utils/reference/iff_all_vals.md):
  Tests if all values are present in a vector and ONLY those values are
  present.

#### Updated

- [`drop_blank()`](https://allenbaron.github.io/DO.utils/reference/drop_blank.md):
  Now a generic with `character` and `list` methods.
- [`vctr_to_string()`](https://allenbaron.github.io/DO.utils/reference/vctr_to_string.md):
  Now always returns `NA` when only input is `NA`, even when
  `na.rm = FALSE`; previously returned `"NA"`.

### DO Management & Analysis

#### New

- [`is_curie()`](https://allenbaron.github.io/DO.utils/reference/is_curie.md):
  Tests for CURIEs in character vectors, according to a specified
  definition that *always* conforms to W3C CURIE Syntax 1.0.
- [`onto_missing()`](https://allenbaron.github.io/DO.utils/reference/onto_missing.md):
  Compares tsv/csv data with data in the ontology to identify data that
  may be missing. Optionally returns data that is present.

#### Updated

- [`robot()`](https://allenbaron.github.io/DO.utils/reference/robot.md)
  errors are now signaled in R and no longer specifies a max heap size
  when using a robot.jar file.
- [`to_curie()`](https://allenbaron.github.io/DO.utils/reference/to_curie.md)/[`to_uri()`](https://allenbaron.github.io/DO.utils/reference/to_uri.md)
  now appropriately remove brackets from URIs and handle delimited
  input.
- [`tidy_sparql()`](https://allenbaron.github.io/DO.utils/reference/tidy_sparql.md)
  has the new `as_curies` argument and converts IRIs to CURIEs by
  default.
- `ns_prefix` now includes more namespace prefixes, including those for
  MeSH and UniProt.
- [`append_to_url()`](https://allenbaron.github.io/DO.utils/reference/append_to_url.md)
  is now vectorized and can append to additional prefixes, including
  anything in `ns_prefix` and URLs commonly used on disease-ontology.org
  for cross-references.
- `format_url()` no longer uses `NA` in the `txt` argument as text
  input.
- [`build_hyperlink()`](https://allenbaron.github.io/DO.utils/reference/build_hyperlink.md)
  takes advantage of the updates to
  [`append_to_url()`](https://allenbaron.github.io/DO.utils/reference/append_to_url.md)
  and `format_url()`.

## DO.utils 0.2.7

### Dependencies

- reticulate updated to \>=v1.28 in an effort to resolve python package
  installation issues; see
  <https://github.com/DiseaseOntology/DO.utils/issues/12>.
- stringr updated to \>= 1.5.0, for access to new `str_escape()`
  function.
- igraph added for new tidygraph/graphml functions. This change doesn’t
  affect much since tidygraph is already a dependency that itself
  depends on igraph.
- Now SUGGESTS the keyring package for API key management.

### Announcements

- Migrated DO.utils repository to the DiseaseOntology organization.
- DO.utils documentation is now available on the web at
  <https://diseaseontology.github.io/DO.utils/> with significant updates
  supporting citation-based assessment of use workflow.

### DO Management & Analysis

#### New

- [`extract_as_tidygraph()`](https://allenbaron.github.io/DO.utils/reference/extract_as_tidygraph.md):
  Extracts nodes and relationships identified in a SPARQL query from
  RDF/XML file and returns it as a tidygraph.
- [`write_graphml()`](https://allenbaron.github.io/DO.utils/reference/write_graphml.md):
  Writes a graph object (tidygraph/iGraph) to a .graphml file.

#### Updated

- *\[BREAKING CHANGE\]*
  [`robot()`](https://allenbaron.github.io/DO.utils/reference/robot.md)
  wrapper function updated to make it easier to use when programming.
- [`plot_branch_counts()`](https://allenbaron.github.io/DO.utils/reference/plot_branch_counts.md)
  now:
  1.  Identifies the count of classes in each branch that are asserted
      or inferred.
  2.  Uses data directly from a local copy of the HumanDiseaseOntology
      repo instead of manually copied release notes.
  3.  Gained the `aspect_ratio` argument.
- [`plot_xref_counts()`](https://allenbaron.github.io/DO.utils/reference/plot_xref_counts.md)
  now uses data directly from a local copy of the HumanDiseaseOntology
  repo instead of manually copied release notes.
- [`plot_citedby()`](https://allenbaron.github.io/DO.utils/reference/plot_citedby.md)
  gained the `color_set` argument to permit more flexible color choice.
- `DO_pubs` updated with 2023 complex disease paper.
  - NOTE: The 2023 Database paper describing the ‘Assessing Resource
    Use’ workflow was *NOT* added because it is not a publication
    describing use of the DO).
- `DO_colors` now include accent colors generated as part of the DO-KB
  addition to the website. These colors are available in standard,
  “\_mid”, and “\_light” versions.

### Assessing Resource Use

#### New

- ‘Assessing Resource Use: Obtaining Use Records’ tutorial/vignette
  added. Describes how to set up DO.utils and execute functions to
  support the ‘Assessing Resource Use’ workflow.
- [`tidy_pub_records()`](https://allenbaron.github.io/DO.utils/reference/tidy_pub_records.md):
  creates a tibble with more limited information from Scopus and PubMed
  references; includes only the columns: `first_author`, `title`,
  `journal`, `pub_date`, `doi`, `pmid`, `scopus_eid`, `pub_type`, and
  `added_dt`.
- [`set_scopus_keys()`](https://allenbaron.github.io/DO.utils/reference/set_scopus_keys.md):
  makes Scopus API key and/or insttoken available for use during an R
  session.
- [`set_entrez_key()`](https://docs.ropensci.org/rentrez/reference/set_entrez_key.html):
  makes Entrez Utils API key available for use during an R session;
  imported from rentrez package.

#### Updated

- [`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
  methods for publication results now include the `added_dt` column in
  output that standardizes how record timestamps are created.
- [`tidy_pubmed_summary()`](https://allenbaron.github.io/DO.utils/reference/tidy_pubmed_summary.md)
  is now *soft deprecated* in favor of
  [`tidy_pub_records()`](https://allenbaron.github.io/DO.utils/reference/tidy_pub_records.md).

### General Utilities

#### Updated

- [`to_range()`](https://allenbaron.github.io/DO.utils/reference/to_range.md)
  now returns `NA` when passed empty vectors.
- [`citedby_scopus()`](https://allenbaron.github.io/DO.utils/reference/citedby_scopus.md)
  has a new `insttoken` argument.
- [`collapse_col()`](https://allenbaron.github.io/DO.utils/reference/collapse_col.md)
  gained all the methods of
  [`collapse_col_flex()`](https://allenbaron.github.io/DO.utils/reference/collapse_col_flex.md),
  along with `na.rm` argument that can be used by all methods.
- [`append_to_url()`](https://allenbaron.github.io/DO.utils/reference/append_to_url.md)
  and
  [`build_hyperlink()`](https://allenbaron.github.io/DO.utils/reference/build_hyperlink.md)
  no longer add a trailing slash to the end of URLs when there is not
  one. Also, a new `sep` argument has been added to provide greater
  control.
- *\[BREAKING CHANGE\]*
  [`format_hyperlink()`](https://allenbaron.github.io/DO.utils/reference/format_hyperlink.md)
  `preserve_NA` argument removed and replaced with `preserve` argument.
  With this change, the output value when a URL is missing will be
  either the URL (i.e. `NA`) or the text passed to `txt`. This allows
  more flexibility in the output to support more use cases.
- [`format_hyperlink()`](https://allenbaron.github.io/DO.utils/reference/format_hyperlink.md)
  now warns when values are passed to `...` when `as` does not equal
  “html” to reduce the likelihood of losing arguments silently.

## DO.utils 0.2.6

Change license to CC0 1.0 Universal to match standard for the Human
Disease Ontology project and in preparation for use in resource use
assessment publication.

## DO.utils 0.2.5

### Deprecations

**Website** \*
[`make_user_list_html()`](https://allenbaron.github.io/DO.utils/reference/make_user_list_html.md)
is deprecated because the user/use case information on
disease-ontology.org was moved from the ‘Collaborators’ page to the new
‘Use Cases’ page. Replaced by
[`make_use_case_html()`](https://allenbaron.github.io/DO.utils/reference/make_use_case_html.md).

### Breaking Changes

**Website** \* `plot_*()` no longer include the datestamp in saved file
names.

**Formatters** \*
[`format_doid()`](https://allenbaron.github.io/DO.utils/reference/format_doid.md)
parameters changed: \* `allow_bare` renamed to `convert_bare` \*
`validate_input` added to allow invalid input to pass-through without
modification.

**Cited by / Search** \*
[`read_pubmed_txt()`](https://allenbaron.github.io/DO.utils/reference/read_pubmed_txt.md)
now parses IDs (PMID, PMCID, & DOI) from citations and returns a
data.frame that includes a record number, IDs, and full citations
instead of a vector of citations.

### Bug Fixes

**Cited by / Search** \*
[`extract_pmid()`](https://allenbaron.github.io/DO.utils/reference/extract_pmid.md)
updated to recognize 1- to 8-long PubMed IDs which should cover the
whole set of actual PMIDs; previously limited to recognizing 8-long
PMIDs. \*
[`as_tibble.esummary_list()`](https://allenbaron.github.io/DO.utils/reference/as_tibble.esummary_list.md)
fixed error due to reduced data output (fewer columns of information)
from
[`pubmed_summary()`](https://allenbaron.github.io/DO.utils/reference/pubmed_summary.md)
caused by API changes. \*
[`match_citations()`](https://allenbaron.github.io/DO.utils/reference/match_citations.md)
now matches DOIs in a case insensitive manner bringing it into
compliance with the [DOI
spec](https://www.doi.org/doi_handbook/2_Numbering.html#2.4).

### Updates

**URLs** \*
[`append_to_url()`](https://allenbaron.github.io/DO.utils/reference/append_to_url.md):
\* Gained a new parameter `preserve_NA`, which allows `NA` values to
pass through instead of being appended to the URL. \* Added ‘github’ and
‘orcid’ as named URLs that might be appended to (via
[`get_url()`](https://allenbaron.github.io/DO.utils/reference/get_url.md)).

**Datasets** \* More prefixes in `ns_prefix` and new prefix subsets
added: \* `not_obo_prefix`: Subset of `ns_prefix` with everything except
OBO ontology prefixes (e.g. ‘dc’, ‘terms’, ‘skos’, ‘owl’, etc.). \*
`obo_prefix`: Subset of `ns_prefix` with standard OBO ontology prefixes
and namespaces. \* `obo_prop_prefix`: New set of prefixes created to
represent frequently used OBO ontology property prefixes. NOTE: There is
one per ontology but these may not exist or may not be the actual
property prefix used by the stated ontology… use with caution.

**General Utilities** \*
[`unique_to_string()`](https://allenbaron.github.io/DO.utils/reference/vctr_to_string.md)/[`vctr_to_string()`](https://allenbaron.github.io/DO.utils/reference/vctr_to_string.md):
added `sort` and other arguments for control of sorting.

### New

**Ontology Extracters/Modifiers** \* `extract_*_axiom()` family: Extract
equivalentClass (‘eq’), subClassOf (‘subclass’), or both (‘class’)
logical axioms. \*
[`queue_xref_split()`](https://allenbaron.github.io/DO.utils/reference/queue_xref_split.md):
Creates a ‘curation queue’ of diseases that may need to split because
they have multiple cross-references from the same source. \*
[`tidy_sparql()`](https://allenbaron.github.io/DO.utils/reference/tidy_sparql.md):
Tidies SPARQL query results.

**Website** \*
[`update_website_count_tables()`](https://allenbaron.github.io/DO.utils/reference/update_website_count_tables.md):
Update counts in tables on ‘DO Imports’ and ‘DO Slims’ pages with data
from a specified release of doid-merged.owl. *Updates data in place*. \*
[`make_use_case_html()`](https://allenbaron.github.io/DO.utils/reference/make_use_case_html.md):
Produces the html for the new ‘Use Cases’ page, split into 3 files, 1
per section for: Ontologies, Resources, and Methodologies. \* *Does not
update data in place.* HTML for the rows & cells must be copied and
pasted over the HTML for each section in the ‘Use Cases’ file. \*
Content is sorted alphabetically by column. \*
[`make_contributor_html()`](https://allenbaron.github.io/DO.utils/reference/make_contributor_html.md):
Produces HTML list of contributors as `<li>` elements for
disease-ontology.org, including links to Github and ORCID, as available.

**URLs** \*
[`format_hyperlink()`](https://allenbaron.github.io/DO.utils/reference/format_hyperlink.md):
Converts URLs into hyperlinks for Google Sheets, Excel, or HTML. \*
[`build_hyperlink()`](https://allenbaron.github.io/DO.utils/reference/build_hyperlink.md):
Shorthand for common
[`append_to_url()`](https://allenbaron.github.io/DO.utils/reference/append_to_url.md)
plus
[`format_hyperlink()`](https://allenbaron.github.io/DO.utils/reference/format_hyperlink.md)
combination.

**Cited by / Search** \* `pub_id_match` (DATA): A named character vector
of regex’s to identify/extract publication IDs (currently PMID, PMCID,
DOI, & Scopus EID).

**General Utilities** \*
[`sandwich_text()`](https://allenbaron.github.io/DO.utils/reference/sandwich_text.md):
Pastes text around strings. \*
[`wrap_onscreen()`](https://allenbaron.github.io/DO.utils/reference/wrap_onscreen.md):
Wraps messages to be printed on the screen. \*
[`invert_sublists()`](https://allenbaron.github.io/DO.utils/reference/invert_sublists.md):
Swaps the list elements between depths 2 & 3, essentially inverting the
grouping. \*
[`lengthen_col()`](https://allenbaron.github.io/DO.utils/reference/lengthen_col.md):
Splits column(s) by a delimiter and lengthens the data.frame so each
value is in its own row. \* **NOTES:** \* This is the reverse of
[`collapse_col()`](https://allenbaron.github.io/DO.utils/reference/collapse_col.md)
but will not recreate the original data.frame after round trip *in most
cases*. \* Uses
[`unnest_cross()`](https://allenbaron.github.io/DO.utils/reference/unnest_cross.md)
internally so the results will always be the cartesian product of
lengthened columns. \*
[`count_delim()`](https://allenbaron.github.io/DO.utils/reference/count_delim.md):
Counts the values in delimited columns; essentially the combination of
[`lengthen_col()`](https://allenbaron.github.io/DO.utils/reference/lengthen_col.md)
and
[`dplyr::count()`](https://dplyr.tidyverse.org/reference/count.html).

**Type Predicates** \*
[`is_valid_obo()`](https://allenbaron.github.io/DO.utils/reference/obo_ID_predicates.md):
Tests whether the elements of a character vector are ‘valid’ OBO Foundry
IDs (based on formatting, not actual existence).

**Formatters** \*
[`format_obo()`](https://allenbaron.github.io/DO.utils/reference/format_obo.md):
Formats OBO Foundry IDs. \*
[`format_axiom()`](https://allenbaron.github.io/DO.utils/reference/format_axiom.md):
Formats OWL functional syntax EQ/SubClassOf axioms to be more human
readable, similar to that of Protege.

## DO.utils 0.2.4

### New

**Testing** \*
[`is_boolean()`](https://allenbaron.github.io/DO.utils/reference/lgl_predicates.md):
T/F type predicate. \*
[`write_access()`](https://allenbaron.github.io/DO.utils/reference/write_access.md):
Test for file write existence with write access.

**Data Conversion** \*
[`to_curie()`](https://allenbaron.github.io/DO.utils/reference/to_curie.md)
&
[`to_uri()`](https://allenbaron.github.io/DO.utils/reference/to_uri.md):
Convert between URI & CURIEs. \*
[`to_range()`](https://allenbaron.github.io/DO.utils/reference/to_range.md):
Convert vector of values to ranges (output as single string).

**Data.frame Manipulation** \*
[`append_empty_col()`](https://allenbaron.github.io/DO.utils/reference/append_empty_col.md):
Add empty columns to a data.frame. \*
[`unnest_cross()`](https://allenbaron.github.io/DO.utils/reference/unnest_cross.md):
Unnest list columns in data.frame *always* creating the cartesian
product. \* Useful for expanding list columns produced by some SPARQL
queries.

**Datasets** \* `ST_pubs`: Information about official publications
describing the Symptom (SYMP) and/or Pathogen Transmission (TRANS)
ontologies. \* Currently, only one conatins one publication. \*
`ns_prefix`: Named character vector of common namespace-prefix pairs
used in DO and/or OBO ontologies.

### Updates

- `cast_to_string` renamed to
  [`collapse_to_string()`](https://allenbaron.github.io/DO.utils/reference/collapse_to_string.md)
- `DO_pubs` now includes `lens_id` with lens.org identifiers for each
  publication.

### Dependencies

- Imports ‘tidyselect’, which is explicitly required for `unnest_cross`
  but is used throughout DO.utils to enable tidyverse-style semantics
  (via dplyr).

## DO.utils 0.2.3

### Cited By – Updates

- [`citedby_pubmed()`](https://allenbaron.github.io/DO.utils/reference/citedby_pubmed.md),
  equivalent to
  [`citedby_scopus()`](https://allenbaron.github.io/DO.utils/reference/citedby_scopus.md),
  now available (uses
  [`citedby_pmid()`](https://allenbaron.github.io/DO.utils/reference/citedby_pmid.md)
  and
  [`pubmed_summary()`](https://allenbaron.github.io/DO.utils/reference/pubmed_summary.md)
  internally.
- [`match_citations()`](https://allenbaron.github.io/DO.utils/reference/match_citations.md)
  improvments:
  - **BREAKING CHANGE** – `add_col` argument changed from boolean to
    `NULL` (replaces `FALSE`) or the name of the column (replaces
    `TRUE`).
  - Bug fixes & message improvements.
- [`extract_pmid()`](https://allenbaron.github.io/DO.utils/reference/extract_pmid.md)
  has a new argument, `no_result`, which provides control over the
  condition signaled when no results are found (error, warning, message,
  or none); previously a simple error was signaled. For any condition
  signaled, there is now an additional class `no_result` to improve
  condition handling.
  - For the `elink` method the default for `no_result` is still “error”
    while for `elink_list` the default is now a warning.
- [`as_tibble.esummary_list()`](https://allenbaron.github.io/DO.utils/reference/as_tibble.esummary_list.md)
  bug caused when some results have no data fixed – caused errors in
  [`as_tibble.esummary_list_nested()`](https://allenbaron.github.io/DO.utils/reference/as_tibble.esummary_list.md)
  precipitated by the tidyverse’s move to more strict vector merging.

**IMPORTANT NOTE:** The “cited by” functionality of `DO.utils` *may no
longer be improved* because recent review of Lens.org results suggests
it may be a good replacement for this PubMed + Scopus search & merge
strategy.

If improvements are made they will likely facilitate one or more of:

- Merging PubMed and Scopus “cited by” results, probably using
  `standardize()` (or similar).
- Reducing data requested from the APIs by implementing timeframe
  parameters in `citedby_*()`.

## DO.utils 0.2.2

- Bug fix to correct error in
  [`format_subtree()`](https://allenbaron.github.io/DO.utils/reference/format_subtree.md)
  when the subtree did *not* have any classes with multi-parentage.
  (Error label: “no fill needed”)

## DO.utils 0.2.1

### New

- Create a text-based subtree/hierarchy.
  - [`extract_subtree()`](https://allenbaron.github.io/DO.utils/reference/extract_subtree.md)
    extracts the data from doid.owl including all descendants and their
    relationships below a specified DOID.
  - [`format_subtree()`](https://allenbaron.github.io/DO.utils/reference/format_subtree.md)
    arranges them in a dataframe as a text-based hierarchy mirroring
    [disease-ontology.org](https://disease-ontology.org/).
  - Primarily designed for creating high quality “tree view” graphics
    similar to EBI’s [Ontology Lookup
    Service](https://www.ebi.ac.uk/ols/ontologies/doid/terms?iri=http%3A%2F%2Fpurl.obolibrary.org%2Fobo%2FDOID_3070).
- Manage DOIDs.
  - [`is_valid_doid()`](https://allenbaron.github.io/DO.utils/reference/obo_ID_predicates.md)
    tests whether inputs are valid DOIDs. Note that mutliple formats are
    considered valid.
  - [`format_doid()`](https://allenbaron.github.io/DO.utils/reference/format_doid.md)
    converts between valid DOID formats.

### Updates

- [`DOrepo()`](https://allenbaron.github.io/DO.utils/reference/DOrepo.md)
  and
  [`owl_xml()`](https://allenbaron.github.io/DO.utils/reference/owl_xml.md)
  no longer fail silently when a file/directory does not exist. pyDOID
  now verifies file paths on instantiation of the underlying objects.

### Dependencies

- Suggests tidygraph, which is required for
  [`format_subtree()`](https://allenbaron.github.io/DO.utils/reference/format_subtree.md).

## DO.utils 0.2.0

### Data

- Added DO Nucleic Acids Research 2022 publication data to `DO_pubs`.

### Dependencies

- R packages:
  - `reticulate` \>= v1.23 required.
  - `tidyr` v1.2.0 introduced breaking changes by requiring “safe” type
    casting (implemented via `vctrs`).
    - [`replace_na.list()`](https://allenbaron.github.io/DO.utils/reference/replace_na.list.md)
      is now deprecated (requires `tidyr` \<= 1.1.4).
- Python dependency: `pyDOID`

### General Purpose

- Created
  [`collapse_col_flex()`](https://allenbaron.github.io/DO.utils/reference/collapse_col_flex.md)
  to collapse data frame columns more flexibly.
  - Adds two new methods beyond “unique”: “first” & “last”.
  - Adds the ability to collapse columns using different methods.
- Created wrapper functions for `pyDOID` classes.
  - [`DOrepo()`](https://allenbaron.github.io/DO.utils/reference/DOrepo.md)
    wraps the `pyDOID.repo.DOrepo` class
  - [`owl_xml()`](https://allenbaron.github.io/DO.utils/reference/owl_xml.md)
    wraps the `pyDOID.owl.xml` class

### Graphics / Website

- Updated:
  - [`plot_citedby()`](https://allenbaron.github.io/DO.utils/reference/plot_citedby.md)
    to a stacked bar chart showing publication types.
  - `DO_colors` to include saturated versions (names prefixed with
    `sat_`).
  - Functions generating html have been updated to match html style
    guide standards.
- Created:
  - [`theme_DO()`](https://allenbaron.github.io/DO.utils/reference/theme_DO.md)
    a `ggplot2` plotting theme for DO.
  - [`plot_def_src()`](https://allenbaron.github.io/DO.utils/reference/plot_def_src.md)
    to display the number of times a source is used to support disease
    definitions in the ontology (designed for
    disease-ontology.org/about/statistics).

### Cited by

- Renamed:
  - `match_citations_fz()` to
    [`match_fz()`](https://allenbaron.github.io/DO.utils/reference/match_fz.md)
  - `concat_pm_citation()` to
    [`read_pubmed_txt()`](https://allenbaron.github.io/DO.utils/reference/read_pubmed_txt.md)
- Updated
  [`match_citations()`](https://allenbaron.github.io/DO.utils/reference/match_citations.md)
  to utilize Scopus EIDs.
- Created:
  - [`pmc_summary()`](https://allenbaron.github.io/DO.utils/reference/pmc_summary.md),
    a parallel to
    [`pubmed_summary()`](https://allenbaron.github.io/DO.utils/reference/pubmed_summary.md)
    that works for PubMed Central.
  - `hoist_ArticleIds()` (internal) - tidies PubMed/PMC identifiers
    - `tidy_ArticleId_set()` (internal)
  - [`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html),
    method `esummary_list_nested`

### URLs

- Created:
  - Functions to read the doid-edit.owl file and extract URLs (+
    helpers).
    - [`read_doid_edit()`](https://allenbaron.github.io/DO.utils/reference/read_doid_edit.md)
    - `extract_doid_url()`
  - Functions designed for URL validation.
    - `validate_url()` + helpers
    - **NOTE:** Helpers for robots.txt respectful validation remain
      *INCOMPLETE* and care should be taken not to overwhelm web servers
      with requests.

## DO.utils 0.1.7

- Setup package to wrap python via `reticulate` package.
- Added functionality to predict mappings using GILDA grounding (a type
  of lexical string matching/natural entity recognition) via python and
  the python modules `pyobo`, `indra`, and `gilda`. New functions:
  - [`pyobo_map()`](https://allenbaron.github.io/DO.utils/reference/pyobo_map.md)
    to create the predicted mappings.
  - [`parse_mapping()`](https://allenbaron.github.io/DO.utils/reference/parse_mapping.md)
    to parse the python.gilda.ScoredMatch results object to a list of
    data frames with matches (1 df/input term).
  - [`unnest_mapping()`](https://allenbaron.github.io/DO.utils/reference/unnest_mapping.md)
    to unnest a list column generated by
    [`pyobo_map()`](https://allenbaron.github.io/DO.utils/reference/pyobo_map.md)
    inside a
    [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
    call; wraps `parse_maping()`.

## DO.utils 0.1.6

### General

- Added **DEPENDENCIES** on `ggplot2`, `googlesheets4`, and `glue`.
- Renamed `match_citations_fz()` to
  [`match_fz()`](https://allenbaron.github.io/DO.utils/reference/match_fz.md).
- Added `cast_to_string()`, a more generalized version of
  [`vctr_to_string()`](https://allenbaron.github.io/DO.utils/reference/vctr_to_string.md)
  that accepts multiple inputs (similar to
  [`paste()`](https://rdrr.io/r/base/paste.html)).
- Added function to
  [`partition()`](https://allenbaron.github.io/DO.utils/reference/chunk.md)
  vectors into groups with `n` elements per group.

### Data

- Added latest official DO publication to `DO_pubs`.
- Added official `DO_colors`.

### Feature: Website Updates

- Added functions to create statistics graphs:
  [`plot_citedby()`](https://allenbaron.github.io/DO.utils/reference/plot_citedby.md),
  `plot_terms_def_counts()`,
  [`plot_branch_counts()`](https://allenbaron.github.io/DO.utils/reference/plot_branch_counts.md),
  [`plot_xref_counts()`](https://allenbaron.github.io/DO.utils/reference/plot_xref_counts.md).
- Added
  [`make_user_list_html()`](https://allenbaron.github.io/DO.utils/reference/make_user_list_html.md)
  to create rows of table in Community \> Collaborators \> Users of the
  Disease Ontology from the DO team’s curated “Uses” Google sheet.

### Internal Use Only

- Added
  [`to_character()`](https://allenbaron.github.io/DO.utils/reference/to_character.md),
  helper for `cast_to_string()`, to reduce lists and data frames to
  character vectors while limiting data loss.
- Added `html_in_rows()`, helper for
  [`make_user_list_html()`](https://allenbaron.github.io/DO.utils/reference/make_user_list_html.md),
  to format html elements in rows (with optional row & cell attributes).
- Added Google sheets identifiers for programmatic access.

## DO.utils 0.1.5

### General

- Added a `NEWS.md` file to track changes to the package.
- Created/updated various **helpers to manage data** (get it, make it
  easier to save/track with version control):
  - [`download_file()`](https://allenbaron.github.io/DO.utils/reference/download_file.md)
    to flexibly handle multiple downloads.
    - Created `download_status` Ref Class to manage downloads based on
      exit code/status.
  - [`confine_list()`](https://allenbaron.github.io/DO.utils/reference/confine_list.md)
    /
    [`release_list()`](https://allenbaron.github.io/DO.utils/reference/confine_list.md)
    to reversibly convert a list column to a character vector (using
    json).
  - [`is_invariant()`](https://allenbaron.github.io/DO.utils/reference/is_invariant.md)
    to test for vectors with only 1 value; with methods for character &
    numeric vectors.
  - [`unique_to_string()`](https://allenbaron.github.io/DO.utils/reference/vctr_to_string.md)
    to collapse vectors to strings.
  - [`unique_if_invariant()`](https://allenbaron.github.io/DO.utils/reference/unique_if_invariant.md)
    to *conditionally* collapse vectors with only 1 value.
  - Added `na.rm` argument to all vctr_to_scalar functions.
  - `replace_*()`
    - `NA`, method for lists.
    - `NULL`, to replace NULL values in lists recursively.
    - `blank`, to replace “” values.
  - [`collapse_col()`](https://allenbaron.github.io/DO.utils/reference/collapse_col.md)
    to collapse 1 or more specified columns in a data frame by
    concatenating unique values together, while preserving unique values
    in all other columns.
- Created
  [`download_obo_ontology()`](https://allenbaron.github.io/DO.utils/reference/download_obo_ontology.md)
  to download 1 or more ontologies maintained by the OBO Foundry.

### Data

- Added more to DO publication info.
- Added OBO Foundry metadata.

### Feature: Alliance

- Made it possible to count Alliance terms for a subset of DOIDs.
- Increased record type count options (arg: record_lvl) –\>
  “full_record”, “disease-object”, “disease”, “object”

### Feature: citedby

- **Created
  [`citedby_scopus()`](https://allenbaron.github.io/DO.utils/reference/citedby_scopus.md)**
  to get cited by publication data from Scopus API, along with s3
  classes & methods to manage Scopus data.
  - Updated to capture datetime citedby data is first retrieved.
- Renamed
  [`citedby_pubmed()`](https://allenbaron.github.io/DO.utils/reference/citedby_pubmed.md)
  to
  [`citedby_pmid()`](https://allenbaron.github.io/DO.utils/reference/citedby_pmid.md)
  to reflect its output.
  - **NEED** new
    [`citedby_pubmed()`](https://allenbaron.github.io/DO.utils/reference/citedby_pubmed.md)
    that combines
    [`citedby_pmid()`](https://allenbaron.github.io/DO.utils/reference/citedby_pmid.md)
    &
    [`pubmed_summary()`](https://allenbaron.github.io/DO.utils/reference/pubmed_summary.md).
- Modified
  [`pubmed_summary()`](https://allenbaron.github.io/DO.utils/reference/pubmed_summary.md)
  to accept PMID lists as input.
  - Uses new
    [`extract_pmid()`](https://allenbaron.github.io/DO.utils/reference/extract_pmid.md)
    `elink_list` method.
- Changed citedby `tidy()` methods to
  [`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
  methods & added new methods.
- Created `truncate_authors()` to shorten long PubMed author lists.
- Created
  [`get_url()`](https://allenbaron.github.io/DO.utils/reference/get_url.md)
  &
  [`append_to_url()`](https://allenbaron.github.io/DO.utils/reference/append_to_url.md)
  to build DOI, PubMed, and PMC URLs for individual publications.
