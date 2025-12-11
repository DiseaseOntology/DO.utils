# Tidy SPARQL Query

Tidies SPARQL query results according to desired specifications (see
`tidy_what` parameter for details).

## Usage

``` r
tidy_sparql(query_res, tidy_what = "everything", ...)
```

## Arguments

- query_res:

  The results of a SPARQL query, as a data.frame (usually produced by
  [`owl_xml()`](https://allenbaron.github.io/DO.utils/reference/owl_xml.md)\$query()
  or similar from
  [`DOrepo()`](https://allenbaron.github.io/DO.utils/reference/DOrepo.md),
  but can also be used to tidy results of [robot("query",
  ...)](https://allenbaron.github.io/DO.utils/reference/robot.md) loaded
  with `readr`).

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

- ...:

  Arguments passed on to
  [`to_curie`](https://allenbaron.github.io/DO.utils/reference/to_curie.md)

  `strip_angle_brackets`

  :   Whether to remove all `<` and `>` from the input, as a boolean
      (default: `TRUE`). Where this might have undesirable consequences,
      e.g. *some* angle brackets need to be removed, perform the removal
      beforehand and use `strip_angle_brackets = FALSE`.
