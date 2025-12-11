# Identify Data Missing from an Ontology (DEPRECATED)

Identifies data missing from an ontology. Currently only works for
mappings, includes both xrefs and skos matches. Requires
[ROBOT](https://robot.obolibrary.org/) for comparison.

## Usage

``` r
onto_missing(onto_path, input, what = "OMIM", report_present = TRUE)
```

## Arguments

- onto_path:

  The path to an ontology -edit file, as a string.

- input:

  The path to a TSV/CSV file with data to compare against the data in
  the ontology, as a string.

- what:

  What to compare between the two files, as a string.

- report_present:

  Whether to report data present in the ontology (default) along with
  data missing, as a boolean.

## Value

For `report_present = TRUE` (default), a list with two data.frames
indicating data in the ontology (`in_onto`) and data missing
(`missing`); otherwise, a single data.frame showing the missing data
only.

## Examples

``` r
if (FALSE) { # \dontrun{
onto_missing(
    onto_path = "~/Ontologies/HumanDiseaseOntology/src/ontology/doid-edit.owl",
    input = "omimps.csv", # e.g. manually copied from https://www.omim.org/phenotypicSeries/PS609060
    "OMIM"
)
} # }
```
