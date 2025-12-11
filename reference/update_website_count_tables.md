# Update Counts in DO Website HTML

Directly updates counts listed in the tables on the disease-ontology.org
"DO Imports" and "DO Slims" pages using data from a specified release.
Changes to these html files should be reviewed and, if correct,
committed to the svn repo for deployment.

## Usage

``` r
update_website_count_tables(DO_repo, tag, svn_repo)
```

## Arguments

- DO_repo:

  The local path to the HumanDiseaseOntology repository, as a string.

- tag:

  The repo tag to extract data from, as a string.

- svn_repo:

  The local path to the DO website svn trunk, as a string.

## Value

Updated counts directly in the html of the svn repo for each page, *as
well as*, the old and new counts for comparison as a list of tibbles
(invisibly).
