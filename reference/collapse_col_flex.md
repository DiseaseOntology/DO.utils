# Collapse Column(s) Flexibly

Collapses values in the column(s) specified using a defined method for
each record, where a record is defined as a unique observation comprised
of all columns *NOT* specified in `collapse`).

## Usage

``` r
collapse_col_flex(df, ..., method = "unique", delim = "|")
```

## Arguments

- df:

  A data.frame

- ...:

  Column-method pairs specifying the `method` to use for each column to
  be collapsed; column names can be bare variables or strings, methods
  must be strings, e.g. column_name = method.

- method:

  A string identifying a function to use; one of "unique", "first", or
  "last"

- delim:

  A delimiter to place between vector elements (default: "\|").

## Value

A data.frame with the specified columns collapsed. Also **NOTE** the
following:

1.  *For all methods*, rows will be reordered by the unique combination
    of columns *not* collapsed (due to
    [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
    use).

2.  \_For "unique" method, collapsed columns **will** be converted to
    `character`.

## See also

[`collapse_col()`](https://allenbaron.github.io/DO.utils/reference/collapse_col.md)
for a simpler approach and
[`lengthen_col()`](https://allenbaron.github.io/DO.utils/reference/lengthen_col.md)
for the pseudo-reverse operation that lengthens/expands one or more
specified columns.

## Examples

``` r
cc_df <- tibble::tibble(
     x = c(1, 2, 3, 3, 4, 4, 4),
     y = c("a", "a", "b", "b", "c", "c", "e"),
     z = c("Z", "Y", "X", "X", "W", "V", "U")
)
cc_df
#> # A tibble: 7 × 3
#>       x y     z    
#>   <dbl> <chr> <chr>
#> 1     1 a     Z    
#> 2     2 a     Y    
#> 3     3 b     X    
#> 4     3 b     X    
#> 5     4 c     W    
#> 6     4 c     V    
#> 7     4 e     U    

# completely duplicated rows (3-4) are collapsed with any column(s) specified
collapse_col_flex(cc_df, y)
#> # A tibble: 6 × 3
#>       x y     z    
#>   <dbl> <chr> <chr>
#> 1     1 a     Z    
#> 2     2 a     Y    
#> 3     3 b     X    
#> 4     4 e     U    
#> 5     4 c     V    
#> 6     4 c     W    

# individual columns
collapse_col_flex(cc_df, z, method = "unique")
#> # A tibble: 5 × 3
#>       x y     z    
#>   <dbl> <chr> <chr>
#> 1     1 a     Z    
#> 2     2 a     Y    
#> 3     3 b     X    
#> 4     4 c     W|V  
#> 5     4 e     U    
collapse_col_flex(cc_df, z, method = "first")
#> # A tibble: 5 × 3
#>       x y     z    
#>   <dbl> <chr> <chr>
#> 1     1 a     Z    
#> 2     2 a     Y    
#> 3     3 b     X    
#> 4     4 c     W    
#> 5     4 e     U    
collapse_col_flex(cc_df, z, method = "last")
#> # A tibble: 5 × 3
#>       x y     z    
#>   <dbl> <chr> <chr>
#> 1     1 a     Z    
#> 2     2 a     Y    
#> 3     3 b     X    
#> 4     4 c     V    
#> 5     4 e     U    

# multiple columns can be collapsed using the same method
collapse_col_flex(cc_df, x, z, method = "unique")
#> # A tibble: 4 × 3
#>   x     y     z    
#>   <chr> <chr> <chr>
#> 1 1|2   a     Z|Y  
#> 2 3     b     X    
#> 3 4     c     W|V  
#> 4 4     e     U    
collapse_col_flex(cc_df, x, z, method = "first")
#> # A tibble: 4 × 3
#>       x y     z    
#>   <dbl> <chr> <chr>
#> 1     1 a     Z    
#> 2     3 b     X    
#> 3     4 c     W    
#> 4     4 e     U    
collapse_col_flex(cc_df, x, z, method = "last")
#> # A tibble: 4 × 3
#>       x y     z    
#>   <dbl> <chr> <chr>
#> 1     2 a     Y    
#> 2     3 b     X    
#> 3     4 c     V    
#> 4     4 e     U    

# ...or using different methods
collapse_col_flex(cc_df, x = "unique", z = "unique")
#> # A tibble: 4 × 3
#>   x     y     z    
#>   <chr> <chr> <chr>
#> 1 1|2   a     Z|Y  
#> 2 3     b     X    
#> 3 4     c     W|V  
#> 4 4     e     U    
collapse_col_flex(cc_df, x = "first", z = "unique")
#> # A tibble: 4 × 3
#>       x y     z    
#>   <dbl> <chr> <chr>
#> 1     1 a     Z|Y  
#> 2     3 b     X    
#> 3     4 c     W|V  
#> 4     4 e     U    
collapse_col_flex(cc_df, x = "first", z = "last")
#> # A tibble: 4 × 3
#>       x y     z    
#>   <dbl> <chr> <chr>
#> 1     1 a     Y    
#> 2     3 b     X    
#> 3     4 c     V    
#> 4     4 e     U    
```
