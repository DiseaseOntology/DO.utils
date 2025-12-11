# Count Delimited Columns

Counts columns when values within them are delimited.

## Usage

``` r
count_delim(
  data,
  ...,
  delim = "|",
  sort = FALSE,
  name = NULL,
  trim = TRUE,
  convert = FALSE
)
```

## Arguments

- data:

  A data.frame.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Variables to first lengthen, then count by.

- delim:

  A delimiter to split elements within specified columns by (default:
  "\|").

- sort:

  If `TRUE`, will show the largest groups at the top.

- name:

  The name of the new column in the output.

  If omitted, it will default to `n`. If there's already a column called
  `n`, it will use `nn`. If there's a column called `n` and `nn`, it'll
  use `nnn`, and so on, adding `n`s until it gets a new name.

- trim:

  Whether to trim start/end whitespace, as a boolean (default: `TRUE`).

- convert:

  Whether to run
  [`utils::type.convert()`](https://rdrr.io/r/utils/type.convert.html)
  with `as.is = TRUE` on new columns. This is useful if the
  de-concatenated columns are integer, numeric or logical. NOTE: "NA"
  strings will *always* be converted to `NA`s.

## Examples

``` r
.df <- tibble::tibble(
    x = 1:3,
    y = c("1|2", "1|3", "2"),
    z = c("1", "2|3", "1|3")
)

# counts undelimited columns like dplyr::count()
count_delim(.df, x)
#> # A tibble: 3 × 2
#>   x         n
#>   <chr> <int>
#> 1 1         1
#> 2 2         1
#> 3 3         1

# counts all delimited values
count_delim(.df, y)
#> # A tibble: 3 × 2
#>   y         n
#>   <chr> <int>
#> 1 1         2
#> 2 2         2
#> 3 3         1

# works for multiple columns that use the same delimiter
count_delim(.df, y, z)
#> # A tibble: 7 × 3
#>   y     z         n
#>   <chr> <chr> <int>
#> 1 1     1         1
#> 2 1     2         1
#> 3 1     3         1
#> 4 2     1         2
#> 5 2     3         1
#> 6 3     2         1
#> 7 3     3         1

# but not those that use different delimiters
.df2 <- dplyr::mutate(.df, z = stringr::str_replace(z, "\\|", "%"))
.df2
#> # A tibble: 3 × 3
#>       x y     z    
#>   <int> <chr> <chr>
#> 1     1 1|2   1    
#> 2     2 1|3   2%3  
#> 3     3 2     1%3  

count_delim(.df2, y, z, delim = "%")
#> # A tibble: 5 × 3
#>   y     z         n
#>   <chr> <chr> <int>
#> 1 1|2   1         1
#> 2 1|3   2         1
#> 3 1|3   3         1
#> 4 2     1         1
#> 5 2     3         1
```
