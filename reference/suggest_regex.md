# Suggest a Regular Expression That Will Match All Input

Collects the full set of characters found at each position across all
strings in `x` and returns it as a quasi-regular expression. Letter and
numbers will not be condensed to ranges in output, even if the full sets
are present at a position.

## Usage

``` r
suggest_regex(x, pivot = "wide")
```

## Arguments

- x:

  A character vector.

- pivot:

  Whether the details `tibble` should be in "wide" (default) or "long"
  format. See Details.

## Value

A string of class `suggested_regex`, including detailed information in a
`details` attribute, which is displayed when printed.

## Details

When `pivot = "long"`, details will be formatted as a tidy `tibble` with
3 columns and as many rows as the string length of the longest input:

1.  `position`: indicating the position of the character set in the
    input.

2.  `regex`: giving the character set (in brackets),

3.  `n`: the count of input strings that have a character at that
    `position`.

When `pivot = "wide"` (default), a `tibble` with the same information
organized into rows (1 header and 2 normal rows) corresponding to the 3
columns described.

## See also

[`print.suggested_regex()`](https://allenbaron.github.io/DO.utils/reference/print.suggested_regex.md)
for the print method.

Other general utilities:
[`length_sort()`](https://allenbaron.github.io/DO.utils/reference/length_sort.md),
[`max_paren_depth()`](https://allenbaron.github.io/DO.utils/reference/max_paren_depth.md),
[`sandwich_text()`](https://allenbaron.github.io/DO.utils/reference/sandwich_text.md)

## Examples

``` r
x <- c("DNA", "MHC", "TAP1", "TAP2", "520", "ACD")

suggest_regex(x)
#> Suggested regex: [5ADMT][2ACHN][0ACDP][12]
#> Details:
#> # A tibble: 2 × 5
#>   position `1`     `2`     `3`     `4`  
#>   <chr>    <chr>   <chr>   <chr>   <chr>
#> 1 regex    [5ADMT] [2ACHN] [0ACDP] [12] 
#> 2 n        4       4       4       2    
suggest_regex(x, "long")
#> Suggested regex: [5ADMT][2ACHN][0ACDP][12]
#> Details:
#> # A tibble: 4 × 3
#>   position regex       n
#>      <int> <chr>   <int>
#> 1        1 [5ADMT]     4
#> 2        2 [2ACHN]     4
#> 3        3 [0ACDP]     4
#> 4        4 [12]        2
```
