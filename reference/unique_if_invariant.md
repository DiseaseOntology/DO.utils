# Return Unique Value for Invariant Vectors

Returns the unique value from an input, if and only if, only 1 unique
value exists (*i.e.* the input is invariant), otherwise returns the
original input. Uniqueness is determined by
[`base::unique()`](https://rdrr.io/r/base/unique.html) for flexibility
but `unique_if_invariant()` may fail for custom methods.

## Usage

``` r
unique_if_invariant(x, na.rm = FALSE, incl_nm = FALSE, ...)
```

## Arguments

- x:

  An R object, except arrays which are not supported.

- na.rm:

  A logical scalar indicating whether `NA` values should be removed
  (default: FALSE); powered by
  [`stats::na.omit()`](https://rdrr.io/r/stats/na.fail.html) and may be
  limited by its methods.

- incl_nm:

  A logic scalar indicating whether names should also be examined
  (default: FALSE).

- ...:

  Arguments passed on to
  [`base::unique()`](https://rdrr.io/r/base/unique.html) methods.

## See also

For *unconditional* vector-to-string conversion methods, see the
[`vctr_to_string()`](https://allenbaron.github.io/DO.utils/reference/vctr_to_string.md)
family of functions.

## Examples

``` r
unique_if_invariant(c("a", "a"))
#> [1] "a"
unique_if_invariant(c("a", "b"))
#> [1] "a" "b"

# `NA` can be ignored
unique_if_invariant(c("a", NA))
#> [1] "a" NA 
unique_if_invariant(c("a", NA), na.rm = TRUE)
#> [1] "a"

# names are ignored by default (and often dropped); to consider and preserve
# them use `incl_nm = TRUE`
unique_if_invariant(c(a = "A", b = "A"))
#> [1] "A"
unique_if_invariant(c(a = "A", b = "A"), incl_nm = TRUE)
#>   a   b 
#> "A" "A" 
unique_if_invariant(c(a = "A", a = "A"), incl_nm = TRUE)
#>   a 
#> "A" 

# na.rm & incl_nm are ignored for matrices & data.frames due to undesirable
# results; as with base::unique(), matrix comparison preserves columns
m <- matrix(rep(1, 4), 2)
unique_if_invariant(m)
#>      [,1] [,2]
#> [1,]    1    1

.df <- data.frame(m, check.names = TRUE)
unique_if_invariant(.df)
#>   X1 X2
#> 1  1  1
```
