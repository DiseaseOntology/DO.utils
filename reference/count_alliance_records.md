# Count Alliance Records

Counts records in data from the Alliance of Genome Resources. Counts can
be ascribed to the species the record is associated with or the Model
Organism Database (MOD) that curated it, optionally by object type.
`count_alliance_records()` was primarily designed to count records in
the Alliance Disease Associations File. *There is no guarantee that
any/all* *options will work for other files.*

## Usage

``` r
count_alliance_records(
  alliance_tbl,
  term_subset = NULL,
  by_type = TRUE,
  pivot = TRUE,
  record_lvl = "disease-object",
  assign_to = c("species", "curator")
)
```

## Arguments

- alliance_tbl:

  a dataframe derived from Alliance data (usually a [downloaded .tsv
  file](https://www.alliancegenome.org/downloads))

- term_subset:

  character vector of DOIDs to limit counts to

- by_type:

  logical indicating whether to count by Alliance object type (i.e.
  gene, allele, model)

- pivot:

  logical indicating whether to pivot values to type columns; ignored if
  by_type = FALSE.

- record_lvl:

  a string indicating the desired specificity of records.

- assign_to:

  how to assign records when counting; one of "species" or "curator"
  (i.e. the organization responsible for curating the record)

## Value

A summary tibble with the count of unique object annotations defined by
`record_lvl`, aggregated according to species/curator (`assign_to`) and,
optionally, object type (`by_type`).

## Details

The type of record information to use in counting should be specified
with `record_lvl` which accepts the following values:

- "full_record" counts full non-duplicate records

- "disease-object" counts unique disease-object combinations

- "disease" counts unique diseases

- "object" counts unique MOD objects (i.e. gene, allele, model
  identifiers)

## NOTE

For disease-related data, some exact duplicates (reason unknown) and
records that differ by seemingly unimportant information (e.g. only the
date differs) have existed. These types of duplicates are removed prior
to record counts.

## See also

Other Alliance functions:
[`download_alliance_tsv()`](https://allenbaron.github.io/DO.utils/reference/download_alliance_tsv.md),
[`read_alliance()`](https://allenbaron.github.io/DO.utils/reference/read_alliance.md),
[`save_alliance_counts()`](https://allenbaron.github.io/DO.utils/reference/save_alliance_counts.md)
