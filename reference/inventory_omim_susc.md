# Assess whether OMIM susceptibilities are in the DO

Assesses whether OMIM entries are present in the Human Disease Ontology
as susceptibilities (in the `omim_susc_import.owl` file). Utilizes
[`robot()`](https://diseaseontology.github.io/DO.utils/reference/robot.md)
for comparison.

## Usage

``` r
inventory_omim_susc(
  susc_path,
  omim_input,
  do_path = NULL,
  keep_mim = c("#", "%")
)
```

## Arguments

- susc_path:

  The path to the `omim_susc_import.owl` file, as a string.

- omim_input:

  An `omim_tbl` created by
  [`read_omim()`](https://diseaseontology.github.io/DO.utils/reference/read_omim.md)
  or the path to a .tsv or .csv file (possibly compressed) that can be
  read by
  [`read_omim()`](https://diseaseontology.github.io/DO.utils/reference/read_omim.md)
  and includes OMIM data to compare against the susceptibility classes
  in the ontology.

  NOTE: If an `omim_tbl` is provided, `keep_mim` will be ignored.

- do_path:

  The path to a Human Disease Ontology file, as a string, or `NULL`
  (default). If provided, additional information about the DOIDs
  (labels, deprecated status) that are related to the susceptibilities
  will be included in the output.

- keep_mim:

  \[**OMIM search data only**\] The MIM symbols representing the data
  types to keep, as a character vector, or `NULL` to retain all
  (default: `"#"` and `"%"`).

  The [OMIM](https://www.omim.org/help/faq#1_3) defined MIM symbols are:

  |            |                                                          |
  |------------|----------------------------------------------------------|
  | MIM symbol | MIM type                                                 |
  | `*`        | gene                                                     |
  | `+`        | gene, includes phenotype                                 |
  | `#`        | descriptive entry, not unique locus; usually a phenotype |
  | `%`        | phenotype / phenotypic locus, unknown molecular basis    |
  | `^`        | deprecated                                               |
  | `none`     | phenotype (usually), suspected or possibly overlapping   |

## Value

The `omim_input` with 4 additional columns:

- `exists`: Logical indicating whether an OMIM ID is present in the DO
  as a susceptibility.

- `susc_label`: The label of the susceptibility.

- `susc_dep`: Logical indicating whether the susceptibility is
  deprecated or not.

- `related_doid`: All disease(s) related to a given OMIM susceptibility
  (delimited by " \| "). If `do_path` is provided, the data will be
  formatted as "label (DOID; deprecated)" for each related disease;
  otherwise, only the DOID(s) will be included.

Output will have the class `omim_susc_inventory`.

## Examples

``` r
if (FALSE) { # \dontrun{
# execute within the HumanDiseaseOntology repository and download data from
# https://www.omim.org/phenotypicSeries/PS145600 to omimps.tsv
inventory_omim_susc(
  susc_path = "src/ontology/omim_susc_import.owl",
  omim_input = "omimps.tsv",
  do_path = "src/ontology/doid-edit.owl"
)
} # }
```
