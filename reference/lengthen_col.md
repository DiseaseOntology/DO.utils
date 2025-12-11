# Lengthen Column(s)

Lengthens values in the column(s) specified, de-concatenating values in
those columns, resulting in duplicated rows that differ only by values
in `cols`. NOTE: `lengthen_col()` is not the exact reverse of
[`collapse_col()`](https://allenbaron.github.io/DO.utils/reference/collapse_col.md);
see examples.

## Usage

``` r
lengthen_col(data, cols, delim = "|", trim = TRUE, convert = FALSE)
```

## Arguments

- data:

  A data.frame.

- cols:

  The name of the column(s) in the data.frame to lengthen.

- delim:

  A delimiter to split elements within specified columns by (default:
  "\|").

- trim:

  Whether to trim start/end whitespace, as a boolean (default: `TRUE`).

- convert:

  Whether to run
  [`utils::type.convert()`](https://rdrr.io/r/utils/type.convert.html)
  with `as.is = TRUE` on new columns. This is useful if the
  de-concatenated columns are integer, numeric or logical. NOTE: "NA"
  strings will *always* be converted to `NA`s.

## Value

A data.frame with the specified columns lengthened.

## See also

[`collapse_col()`](https://allenbaron.github.io/DO.utils/reference/collapse_col.md)
or
[`collapse_col_flex()`](https://allenbaron.github.io/DO.utils/reference/collapse_col_flex.md)
for pseudo-reverse operations that collapse one or more specified
columns.

## Examples

``` r
z_unique <- tibble::tibble(
  x = c(1, 2, 3, 4, 4),
  y = c("a", "a", "b", "c", "e"),
  z = c("Z", "Y", "X", "W|V", "U")
)

lengthen_col(z_unique, z)
#> # A tibble: 6 × 3
#>       x y     z    
#>   <dbl> <chr> <chr>
#> 1     1 a     Z    
#> 2     2 a     Y    
#> 3     3 b     X    
#> 4     4 c     W    
#> 5     4 c     V    
#> 6     4 e     U    

# Data will likely differ after round trip through `collapse_col()` and
# `lengthen_col()` because:
#    1. Duplicate rows (cc_df, row 4) are lost
#    2. New crosses are created (cc_df2, row 6-8)
#    3. Output is sorted by `cols` due to use of `dplyr::group_by()`
#       internally.
cc_df <- tibble::tibble(
  x = c(1, 2, 3, 3, 4, 4, 4),
  y = c("a", "a", "b", "b", "c", "c", "e"),
  z = c("Z", "Y", "X", "X", "W", "V", "U")
)

cc_df2 <- lengthen_col(
  collapse_col(cc_df, c(y, z)),
  c(y, z)
)

if (rlang::is_installed("waldo")) {
  waldo::compare(cc_df, cc_df2)
} else {
  cc_df
  cc_df2
}
#> `attr(old, 'row.names')[5:7]`: 5 6 7    
#> `attr(new, 'row.names')[5:9]`: 5 6 7 8 9
#> 
#> old vs new
#>            x y z
#>   old[1, ] 1 a Z
#>   old[2, ] 2 a Y
#>   old[3, ] 3 b X
#> - old[4, ] 3 b X
#>   old[5, ] 4 c W
#>   old[6, ] 4 c V
#> + new[6, ] 4 c U
#> + new[7, ] 4 e W
#> + new[8, ] 4 e V
#>   old[7, ] 4 e U
#> 
#> `old$x[4:7]`: 3           4 4 4
#>      `new$x`: 1 2 3 4 4 4 4 4 4
#> 
#> `old$y[4:7]`: "b"                     "c" "c" "e"
#> `new$y`:      "a" "a" "b" "c" "c" "c" "e" "e" "e"
#> 
#> `old$z`: "Z" "Y" "X" "X"         "W" "V" "U"
#> `new$z`: "Z" "Y" "X" "W" "V" "U" "W" "V" "U"
```
