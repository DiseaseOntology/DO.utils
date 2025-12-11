# Replace Counts in HTML File

Replaces counts in the specified page of disease-ontology.org.
`replace_html_counts()` is the primary workhorse supporting the more
generalized
[`update_website_count_tables()`](https://allenbaron.github.io/DO.utils/reference/update_website_count_tables.md).

## Usage

``` r
replace_html_counts(DO_repo, svn_repo, page, reload = NULL)
```

## Arguments

- DO_repo:

  The local path to the HumanDiseaseOntology repository, as a string.

- svn_repo:

  The local path to the DO website svn trunk, as a string.

- page:

  The disease-ontology.org page in which to replace counts, as a string;
  either "imports" or "slims".

- reload:

  *DEPRECATED*. This argument is now ignored and will be removed in a
  future release.

## Value

Updated counts directly in the html of the svn repo for the specified
page, as well as the old and new counts for comparison (invisibly, as a
tibble).
