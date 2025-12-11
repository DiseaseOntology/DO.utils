# Sort by Character Length

Sort a vector (`length_sort()`) or data.frame (`length_order()`) by
character length. Multiple elements of the the same length are
secondarily sorted by order of appearance.

## Usage

``` r
length_sort(x, by_name = FALSE, ...)

length_order(data, cols, ...)
```

## Arguments

- x:

  A vector.

- by_name:

  Whether to sort a vector by name instead of value, as a boolean.

- ...:

  Arguments passed on to
  [`base::order`](https://rdrr.io/r/base/order.html)

  `decreasing`

  :   logical. Should the sort order be increasing or decreasing? For
      the `"radix"` method, this can be a vector of length equal to the
      number of arguments in `...` and the elements are recycled as
      necessary. For the other methods, it must be length one.

  `na.last`

  :   for controlling the treatment of `NA`s. If `TRUE`, missing values
      in the data are put last; if `FALSE`, they are put first; if `NA`,
      they are removed (see ‘Note’.)

  `method`

  :   the method to be used: partial matches are allowed. The default
      (`"auto"`) implies `"radix"` for numeric vectors, integer vectors,
      logical vectors and factors with fewer than \\2^{31}\\ elements.
      Otherwise, it implies `"shell"`. For details of methods `"shell"`,
      `"quick"`, and `"radix"`, see the help for
      [`sort`](https://rdrr.io/r/base/sort.html).

- data:

  A data.frame.

- cols:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  The columns of `data` to order by.

## See also

Other general utilities:
[`max_paren_depth()`](https://allenbaron.github.io/DO.utils/reference/max_paren_depth.md),
[`sandwich_text()`](https://allenbaron.github.io/DO.utils/reference/sandwich_text.md),
[`suggest_regex()`](https://allenbaron.github.io/DO.utils/reference/suggest_regex.md)

## Examples

``` r
# Sorting vectors
x <- c("ccc", "aaaa", "eee", "b", "DDD")
length_sort(x)
#> [1] "b"    "ccc"  "eee"  "DDD"  "aaaa"
length_sort(x, decreasing = TRUE)
#> [1] "aaaa" "ccc"  "eee"  "DDD"  "b"   

x2 <- c(1:9, NA, 100, 10)
length_sort(x2)
#>  [1]   1   2   3   4   5   6   7   8   9  10 100  NA
length_sort(x2, decreasing = TRUE)
#>  [1] 100  10   1   2   3   4   5   6   7   8   9  NA
length_sort(x2, na.last = NA)
#>  [1]   1   2   3   4   5   6   7   8   9  10 100

x3 <- c(bb = 333, ccc = 1, a = 22)
length_sort(x3, by_name = TRUE)
#>   a  bb ccc 
#>  22 333   1 

# Ordering data.frames
x <- tibble::tibble(
    x = 1:3,
    y = c("b", "aa", "c"),
    z = c("bb", "a", "c")
)

length_order(x, "y")
#> # A tibble: 3 × 3
#>       x y     z    
#>   <int> <chr> <chr>
#> 1     1 b     bb   
#> 2     3 c     c    
#> 3     2 aa    a    
length_order(x, "z")
#> # A tibble: 3 × 3
#>       x y     z    
#>   <int> <chr> <chr>
#> 1     2 aa    a    
#> 2     3 c     c    
#> 3     1 b     bb   
length_order(x, c("y", "z"))
#> # A tibble: 3 × 3
#>       x y     z    
#>   <int> <chr> <chr>
#> 1     3 c     c    
#> 2     1 b     bb   
#> 3     2 aa    a    
length_order(x, c("y", "z"), decreasing = TRUE)
#> # A tibble: 3 × 3
#>       x y     z    
#>   <int> <chr> <chr>
#> 1     2 aa    a    
#> 2     1 b     bb   
#> 3     3 c     c    
```
