# Collapse Column(s)

Collapses values in the column(s) specified, concatenating unique values
in those columns for each record (where a record is defined as a unique
row including all columns *NOT* specified in `.cols`).

## Usage

``` r
collapse_col(df, .cols, delim = "|", method = "unique", na.rm = FALSE)
```

## Arguments

- df:

  a data.frame

- .cols:

  the name of the column in the data.frame to collapse

- delim:

  A delimiter to place between vector elements (default: "\|").

- method:

  A string identifying a function to use; one of "unique", "first", or
  "last".

- na.rm:

  A logical scalar indicating whether `NA` values should be removed
  (default: `FALSE`).

## Value

A data.frame with the specified columns collapsed. Also **NOTE** the
following:

1.  Collapsed columns **will** be converted to `character`.

2.  Rows will be reordered by the unique combination of columns *not*
    collapsed (due to
    [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
    use).

## See also

[`collapse_col_flex()`](https://allenbaron.github.io/DO.utils/reference/collapse_col_flex.md)
for a more flexible approach and
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

# completely duplicated rows (3-4) are collapsed with any .cols specified
collapse_col(cc_df, y)
#> # A tibble: 6 × 3
#>       x y     z    
#>   <dbl> <chr> <chr>
#> 1     1 a     Z    
#> 2     2 a     Y    
#> 3     3 b     X    
#> 4     4 e     U    
#> 5     4 c     V    
#> 6     4 c     W    

collapse_col(cc_df, x)
#> # A tibble: 6 × 3
#>   x     y     z    
#>   <chr> <chr> <chr>
#> 1 2     a     Y    
#> 2 1     a     Z    
#> 3 3     b     X    
#> 4 4     c     V    
#> 5 4     c     W    
#> 6 4     e     U    
collapse_col(cc_df, z)
#> # A tibble: 5 × 3
#>       x y     z    
#>   <dbl> <chr> <chr>
#> 1     1 a     Z    
#> 2     2 a     Y    
#> 3     3 b     X    
#> 4     4 c     W|V  
#> 5     4 e     U    
collapse_col(cc_df, c(x, z))
#> # A tibble: 4 × 3
#>   x     y     z    
#>   <chr> <chr> <chr>
#> 1 1|2   a     Z|Y  
#> 2 3     b     X    
#> 3 4     c     W|V  
#> 4 4     e     U    

# negative & tidy selection works; all equivalent to collapse_col(cc_df, c(x, z))
collapse_col(cc_df, -y)
#> # A tibble: 4 × 3
#>   x     y     z    
#>   <chr> <chr> <chr>
#> 1 1|2   a     Z|Y  
#> 2 3     b     X    
#> 3 4     c     W|V  
#> 4 4     e     U    
collapse_col(cc_df, dplyr::matches("x|z"))
#> # A tibble: 4 × 3
#>   x     y     z    
#>   <chr> <chr> <chr>
#> 1 1|2   a     Z|Y  
#> 2 3     b     X    
#> 3 4     c     W|V  
#> 4 4     e     U    
collapse_col(cc_df, -dplyr::all_of("y"))
#> # A tibble: 4 × 3
#>   x     y     z    
#>   <chr> <chr> <chr>
#> 1 1|2   a     Z|Y  
#> 2 3     b     X    
#> 3 4     c     W|V  
#> 4 4     e     U    
```
