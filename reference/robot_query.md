# Execute a SPARQL Query with ROBOT

Wrapper for `robot("query", ...)` that accepts a file *or* text query,
and has more convenient arguments.

## Usage

``` r
robot_query(
  input,
  query,
  output = NULL,
  ...,
  tidy_what = "nothing",
  col_types = NULL,
  .robot_path = NULL
)
```

## Arguments

- input:

  The path to an RDF/OWL file recognized by ROBOT, as a string.

- query:

  The text for or path to a valid SPARQL query (`ASK`, `SELECT`,
  `CONSTRUCT`, or `UPDATE`) as a string.

- output:

  The path where output will be written, as a string, or `NULL`
  (default) to load data directly. `output` is required for `UPDATE` and
  `CONSTRUCT` queries.

- ...:

  Additional arguments to [ROBOT
  query](http://robot.obolibrary.org/query) formatted as described in
  [`robot()`](https://allenbaron.github.io/DO.utils/reference/robot.md).

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

- col_types:

  One of `NULL`, a
  [`cols()`](https://readr.tidyverse.org/reference/cols.html)
  specification, or a string. See `vignette("readr")` for more details.

  If `NULL`, all column types will be inferred from `guess_max` rows of
  the input, interspersed throughout the file. This is convenient (and
  fast), but not robust. If the guessed types are wrong, you'll need to
  increase `guess_max` or supply the correct types yourself.

  Column specifications created by
  [`list()`](https://rdrr.io/r/base/list.html) or
  [`cols()`](https://readr.tidyverse.org/reference/cols.html) must
  contain one column specification for each column. If you only want to
  read a subset of the columns, use
  [`cols_only()`](https://readr.tidyverse.org/reference/cols.html).

  Alternatively, you can use a compact string representation where each
  character represents one column:

  - c = character

  - i = integer

  - n = number

  - d = double

  - l = logical

  - f = factor

  - D = date

  - T = date time

  - t = time

  - ? = guess

  - \_ or - = skip

  By default, reading a file without a column specification will print a
  message showing what `readr` guessed they were. To remove this
  message, set `show_col_types = FALSE` or set
  \`options(readr.show_col_types = FALSE).

- .robot_path:

  The path to a ROBOT executable or .jar file, as a string. When `NULL`
  (default), if a system ROBOT executable is available it will be used,
  otherwise an error will be signaled.

  **NOTE:** `DO.utils` caches the last ROBOT used for future use.

## Value

If `output` is specified, the path to the output file with the query
result. Otherwise, the query result (ASK as boolean or SELECT as
`tibble`).

## See also

[`robot()`](https://allenbaron.github.io/DO.utils/reference/robot.md)
for underlying implementation.
