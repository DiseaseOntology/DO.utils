# Unnest data frame list columns

Like
[`tidyr::unnest()`](https://tidyr.tidyverse.org/reference/unnest.html)
but always produces a cartesian product and does not requires that list
columns be "parallel entries ... of compatible sizes".

## Usage

``` r
unnest_cross(data, cols, ...)
```

## Arguments

- data:

  A data.frame.

- cols:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  Columns to unnest.

- ...:

  Arguments passed on to
  [`tidyr::unchop`](https://tidyr.tidyverse.org/reference/chop.html),
  [`tidyr::unpack`](https://tidyr.tidyverse.org/reference/pack.html)

  `error_call`

  :   The execution environment of a currently running function, e.g.
      `caller_env()`. The function will be mentioned in error messages
      as the source of the error. See the `call` argument of
      [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
      information.

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

## Examples

``` r
df <- tibble::tibble(
    n = 1:2,
    interjection = list(c("oh", "wow"), "zap"),
    letter = list(c("a", "b", "c"), "d"),
    keep_list = list("please", c("don't", "unnest"))
)
df
#> # A tibble: 2 × 4
#>       n interjection letter    keep_list
#>   <int> <list>       <list>    <list>   
#> 1     1 <chr [2]>    <chr [3]> <chr [1]>
#> 2     2 <chr [1]>    <chr [1]> <chr [2]>

# Uses tidyselect semantics, like tidyr::unnest()
unnest_cross(df, cols = tidyselect::starts_with("inter"))
#> # A tibble: 3 × 4
#>       n interjection letter    keep_list
#>   <int> <chr>        <list>    <list>   
#> 1     1 oh           <chr [3]> <chr [1]>
#> 2     1 wow          <chr [3]> <chr [1]>
#> 3     2 zap          <chr [1]> <chr [2]>

# Works when list column sizes differ, unlike tidyr::unnest()
tryCatch(
    tidyr::unnest(df, cols = c(interjection, letter)),
    error = function(e) message(e)
)
#> Error in tidyr::unnest(df, cols = c(interjection, letter)): In row 1, can't recycle input of size 2 to size 3.
unnest_cross(df, cols = c(interjection, letter))
#> # A tibble: 7 × 4
#>       n interjection letter keep_list
#>   <int> <chr>        <chr>  <list>   
#> 1     1 oh           a      <chr [1]>
#> 2     1 oh           b      <chr [1]>
#> 3     1 oh           c      <chr [1]>
#> 4     1 wow          a      <chr [1]>
#> 5     1 wow          b      <chr [1]>
#> 6     1 wow          c      <chr [1]>
#> 7     2 zap          d      <chr [2]>

# Always produces a cartesian product, unlike tidyr::unnest()
df2 <- tibble::tibble(
    n = list(1:2, 3L),
    letter = list(c("a", "b"), "c"),
)
df2
#> # A tibble: 2 × 2
#>   n         letter   
#>   <list>    <list>   
#> 1 <int [2]> <chr [2]>
#> 2 <int [1]> <chr [1]>
tidyr::unnest(df2, cols = tidyselect::everything())
#> # A tibble: 3 × 2
#>       n letter
#>   <int> <chr> 
#> 1     1 a     
#> 2     2 b     
#> 3     3 c     
unnest_cross(df2, cols = tidyselect::everything())
#> # A tibble: 5 × 2
#>       n letter
#>   <int> <chr> 
#> 1     1 a     
#> 2     1 b     
#> 3     2 a     
#> 4     2 b     
#> 5     3 c     
```
