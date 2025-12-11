# Append Empty Columns

Appends empty columns (value = `NA`) to a data.frame.

## Usage

``` r
append_empty_col(df, col, order = FALSE)
```

## Arguments

- df:

  A data.frame.

- col:

  The name(s) of one or more columns desired in the final data.frame, as
  a character vector. Any names not currently in the data.frame will be
  added as empty columns; those present will remain unchanged.

- order:

  Whether to reorder the data.frame to match the order of `col`, as a
  boolean (default: `FALSE`). If `FALSE` empty columns are added to the
  right. When `order = TRUE`, `col` is used to specify column order.
  Thus, in addition to the names of the empty columns to append, `col`
  must include all column names in `df`. `append_empty_col()` will not
  subset/select columns.
