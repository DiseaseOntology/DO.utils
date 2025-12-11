# Assess whether OMIM Data is in DO

Assesses whether OMIM identifiers are present in the Human Disease
Ontology as mappings (either xrefs or skos mappings). Utilizes
[`robot()`](https://allenbaron.github.io/DO.utils/reference/robot.md)
for comparison.

## Usage

``` r
inventory_omim(
  onto_path,
  omim_input,
  keep_mim = c("#", "%"),
  include_pred = c("skos:exactMatch", "skos:closeMatch", "oboInOwl:hasDbXref"),
  when_pred_NA = "error"
)
```

## Arguments

- onto_path:

  The path to an ontology file, as a string.

- omim_input:

  An `omim_tbl` created by
  [`read_omim()`](https://allenbaron.github.io/DO.utils/reference/read_omim.md)
  or the path to a .tsv or .csv file (possibly compressed) that can be
  read by
  [`read_omim()`](https://allenbaron.github.io/DO.utils/reference/read_omim.md)
  and includes OMIM data to compare against the mappings in the
  ontology.

  NOTE: If an `omim_tbl` is provided, `keep_mim` will be ignored.

- keep_mim:

  \[**OMIM search data only**\] The MIM symbols representing the data
  types to keep, as a character vector, or `NULL` to retain all
  (default: `"#"` and `"%"`).

  The [OMIM](https://www.omim.org/help/faq#1_3) defined MIM symbols are:

  |            |                                    |
  |------------|------------------------------------|
  | MIM symbol | MIM type                           |
  | `*`        | gene                               |
  | `+`        | gene, includes phenotype           |
  | `#`        | phenotype                          |
  | `%`        | phenotype, unknown molecular basis |
  | `^`        | deprecated                         |
  | `none`     | phenotype, suspected/overlap       |

- include_pred:

  The predicates to include when testing for one-to-multiple mappings,
  as a character vector (default: `skos:exactMatch`, `skos:closeMatch`,
  and `oboInOwl:hasDbXref`). All other predicates are ignored.

- when_pred_NA:

  What to do when missing predicates are detected, as a string; one of
  "error" (default), "warn", or NULL (do nothing). `NA` predicates are
  *always* ignored when no mapping exists (i.e. one or both
  corresponding values of `x` or `y` is/are also `NA`).

## Value

The `omim_input` with 5 additional columns:

- `exists`: Logical indicating whether an OMIM ID is present in the DO.

- `mapping_type`: The mapping predicate(s) of this OMIM ID to a disease,
  if present. Multiple predicate(s) between the same OMIM and DOID will
  be pipe delimited.

- `doid`: The DOID of the disease mapped to this OMIM ID, if present.

- `do_label`: The label of the disease mapped to this OMIM ID, if
  present.

- `do_dep`: Logical indicating whether a disease is deprecated or not,
  if present.

- `multimaps`: The direction in which an OMIM or DO term maps to
  multiple terms in the other resource, as "omim_to_doid",
  "doid_to_omim", "both_ways" or `NA`.

Output will have the class `omim_inventory`, a type of class
`mapping_inventory`.

## Examples

``` r
if (FALSE) { # \dontrun{
# manually copy or download data from https://www.omim.org/phenotypicSeries/PS609060
inventory_omim(
    onto_path = "~/Ontologies/HumanDiseaseOntology/src/ontology/doid-edit.owl",
    omim_input = "omimps.csv",
)
} # }
```
