# Extract mappings from ORDO

Extract mappings from the Orphanet Rare Disease Ontology (ORDO). ORDO
uses `oboInOwl:hasDbXref` for mapping with annotations to indicate
exact/broad/narrow-ness. Utilizes
[`robot()`](https://allenbaron.github.io/DO.utils/reference/robot.md).

## Usage

``` r
extract_ordo_mappings(
  ordo_path,
  as_skos = TRUE,
  output = NULL,
  tidy_what = "everything"
)
```

## Arguments

- ordo_path:

  The path to the ORDO file, as a string.

- as_skos:

  Whether to convert ORDO's annotated `oboInOwl:hasDbXref` mappings to
  their [Simple Knowledge Organization System
  (SKOS)](https://www.w3.org/TR/2009/REC-skos-reference-20090818/)
  equivalents, as a boolean (default: `TRUE`).

  The ORDO-skos equivalent predicates are as follows:

  - `"BTNT"` - `skos:narrowMatch`

  - `"NTBT"` - `skos:broadMatch`

  - `"E"` - `skos:exactMatch`

  - `"ND"` - `doid:undefinedMatch` (supplements SKOS)

  - `"W"` - `doid:notMatch` (supplements SKOS)

- output:

  The path where output will be written, as a string, or `NULL`
  (default) to load data directly.

- tidy_what:

  The elements of a SPARQL-created data.frame to tidy, as a character
  vector. One or more of the following:

  - `"everything"` to apply all tidy operations (has precedence over
    `"nothing"`).

  - `"header"` to remove leading `?` from header labels.

  - `"unnest"` to unnest list columns with
    [`unnest_cross()`](https://allenbaron.github.io/DO.utils/reference/unnest_cross.md).

  - `"uri_to_curie"` to convert all URIs recognized by DO.utils to
    CURIEs with
    [`to_curie()`](https://allenbaron.github.io/DO.utils/reference/to_curie.md).

  - `"lgl_NA_FALSE"` to replace `NA` in logical columns with `FALSE`.

  - `"as_tibble"` to make the output a
    [tibble](https://tibble.tidyverse.org/reference/tibble.html).

  - `"rm_lang_tag"` to remove language tags. Tags will only be removed
    from `character` class columns, and then only if there is one unique
    language tag in the given column.

  - `"nothing"` to prevent all tidying.

## Value

If `output` is specified, the path to the output file with the data.
Otherwise, the data as a
[tibble](https://tibble.tidyverse.org/reference/tibble.html).

ORDO mappings data will be formatted according to the
[SSSOM](https://github.com/mapping-commons/sssom) specification, with an
additional `status` column indicating the status (active, deprecated,
etc.) of each ORPHA term.

If `as_skos = FALSE`, ORDO's text-based `oboInOwl:hasDbXref` annotations
denoting the type of relationship the Xref represents (simple text code
only) will be included in the `predicate_modifier` column.
