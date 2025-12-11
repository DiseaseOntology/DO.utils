# Calculate Maximum Parentheses Depth

Calculates the maximum depth of nested parentheses in a string or
character vector.

## Usage

``` r
max_paren_depth(x, unmatched_err = TRUE)
```

## Arguments

- x:

  A character vector.

- unmatched_err:

  Whether to signal an error or return `NA` for strings with unmatched
  parentheses,

## Value

An integer vector of the same length as the input giving the maximum
depth of nested parentheses in each element. `NA` will bStrings with
unmatched parentheses will return `NA`

## See also

Other general utilities:
[`length_sort()`](https://allenbaron.github.io/DO.utils/reference/length_sort.md),
[`sandwich_text()`](https://allenbaron.github.io/DO.utils/reference/sandwich_text.md),
[`suggest_regex()`](https://allenbaron.github.io/DO.utils/reference/suggest_regex.md)

## Examples

``` r
max_paren_depth(c("no parens", "a (1 deep)", "((a) and (b or (3 deep)))"))
#>                 no parens                a (1 deep) ((a) and (b or (3 deep))) 
#>                         0                         1                         3 

# errs by default
tryCatch(
  max_paren_depths("unmatched ( paren"),
  error = function(e) print(e$message)
)
#> [1] "could not find function \"max_paren_depths\""

# allow `NA` output for unmatched parentheses
max_paren_depth("unmatched ( paren", unmatched_err = FALSE)
#> unmatched ( paren 
#>                NA 
max_paren_depth(
  c("a (1 deep)", "((a) and (b or (3 deep)))", "unmatched ( paren"),
  unmatched_err = FALSE
)
#>                a (1 deep) ((a) and (b or (3 deep)))         unmatched ( paren 
#>                         1                         3                        NA 
```
