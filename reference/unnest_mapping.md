# Unnest Mapping

Tidies
[`pyobo_map()`](https://allenbaron.github.io/DO.utils/reference/pyobo_map.md)
results stored in the column of a data frame in two steps:

1.  Extracts mapping results from specialized `ScoredMatch` python
    objects (as defined by GILDA).

2.  Unnests the results (from list of data frames).

## Usage

``` r
unnest_mapping(
  df,
  col,
  prefix = NULL,
  prefix_sep = ":",
  best_only = TRUE,
  warn_best_gt1 = FALSE,
  ...
)
```

## Arguments

- df:

  A data.frame.

- col:

  The column with
  [`pyobo_map()`](https://allenbaron.github.io/DO.utils/reference/pyobo_map.md)
  results, as a [tidy-select
  specification](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html).

- prefix:

  *Optional* prefix to add to namespace local unique identifiers (LUI;
  e.g. 4, the LUI for "disease" in DO), as a string; preferably to
  create a complete namespace ID (e.g. "DOID:4").

- prefix_sep:

  *Optional* separator placed between `prefix` and namespace LUIs, as a
  string. Ignored if `prefix = NULL`.

- best_only:

  Whether to return the best scoring result(s) only, as a boolean.
  `FALSE` will return all results. **NOTE:** `TRUE` will return more
  than one result in the case of ties.

- warn_best_gt1:

  Whether to warn that best mapping ties exist for a term.

- ...:

  Arguments passed on to
  [`tidyr::unnest`](https://tidyr.tidyverse.org/reference/unnest.html)

  `keep_empty`

  :   By default, you get one row of output for each element of the list
      that you are unchopping/unnesting. This means that if there's a
      size-0 element (like `NULL` or an empty data frame or vector),
      then that entire row will be dropped from the output. If you want
      to preserve all rows, use `keep_empty = TRUE` to replace size-0
      elements with a single row of missing values.

  `ptype`

  :   Optionally, a named list of column name-prototype pairs to coerce
      `cols` to, overriding the default that will be guessed from
      combining the individual values. Alternatively, a single empty
      ptype can be supplied, which will be applied to all `cols`.

  `names_sep`

  :   If `NULL`, the default, the outer names will come from the inner
      names. If a string, the outer names will be formed by pasting
      together the outer and the inner column names, separated by
      `names_sep`.

  `names_repair`

  :   Used to check that output data frame has valid names. Must be one
      of the following options:

      - `"minimal`": no name repair or checks, beyond basic existence,

      - `"unique`": make sure names are unique and not empty,

      - `"check_unique`": (the default), no name repair, but check they
        are unique,

      - `"universal`": make the names unique and syntactic

      - a function: apply custom name repair.

      - [tidyr_legacy](https://tidyr.tidyverse.org/reference/tidyr_legacy.html):
        use the name repair from tidyr 0.8.

      - a formula: a purrr-style anonymous function (see
        [`rlang::as_function()`](https://rlang.r-lib.org/reference/as_function.html))

      See
      [`vctrs::vec_as_names()`](https://vctrs.r-lib.org/reference/vec_as_names.html)
      for more details on these terms and the strategies used to enforce
      them.

## Value

The input data frame with additional columns `id`, `term` (namespace
label), and `score` (mapping score as determined by GILDA). The data
frame will have additional rows if `best_score = FALSE` or ties for best
score exist for a term.
