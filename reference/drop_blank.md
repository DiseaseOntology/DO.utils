# Drop Blanks

Drops blank values from a vector or list. See
[`is_blank()`](https://allenbaron.github.io/DO.utils/reference/char_val_predicates.md)
for what constitutes a blank value. `drop_blank()` is a generic
function.

## Usage

``` r
drop_blank(x)
```

## Arguments

- x:

  A character vector or list.

## Value

An object of the same class and length as `x`.

## Examples

``` r
drop_blank(c("", "A"))
#> [1] "A"
drop_blank(
    list(
        c("", "A"),
        c("A", "B"),
        c("C", "D", "", "E", "")
    )
)
#> [[1]]
#> [1] "A"
#> 
#> [[2]]
#> [1] "A" "B"
#> 
#> [[3]]
#> [1] "C" "D" "E"
#> 
```
