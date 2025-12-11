# Extract Mappings from an OBO Foundry Ontology

Extract mappings from the OBO Foundry Ontology as SSSOM. .

## Usage

``` r
extract_obo_mappings(
  onto_path,
  id = NULL,
  version_as = "release",
  output = NULL,
  tidy_what = c("header", "unnest"),
  split_prefix_date = TRUE
)
```

## Arguments

- onto_path:

  The path to an ontology file recognizable by
  [ROBOT](https://robot.obolibrary.org/), as a string.

- id:

  A character vector of IRIs or CURIEs to filter results to or `NULL`
  (default) to return all mappings.

- version_as:

  The format for `subject_source_version`, if a `versionIRI` is found,
  as a string. One of:

  - `"release"` (default): Use the ontology's release version as a
    string. Assumes that the release version in the `versionIRI` follows
    `release/` or `releases/`. Will return full `versionIRI` as
    fallback.

  - `"iri"`: Use the ontology's full `versionIRI`.

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

- split_prefix_date:

  Whether to extract date suffixes, which may exist in some
  `object_id`s, out as the `object_source_version` and remove them from
  the `object_id`, as a boolean (default: `TRUE`).

## Value

If `output` is specified, the path to the output file with the data.
Otherwise, the data as a
[tibble](https://tibble.tidyverse.org/reference/tibble.html).

Mappings data will be formatted according to the
[SSSOM](https://github.com/mapping-commons/sssom) specification, with an
additional `status` column indicating the status (active, deprecated,
etc.) of each `subject_id`.

`subject_source_version` will be the ontology `versionIRI` (formatted
according to `version_as` or today's date if `versionIRI` is not
defined.
