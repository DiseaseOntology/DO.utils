# Calculate a Rolling/Windowed Middle (Mean) Value

Calculates a rolling/windowed mean between two values using an
additional `limit` value for the first or last calculation.

## Usage

``` r
roll_middle(x, limit, limit_type = "min")
```

## Arguments

- x:

  A numeric vector.

- limit:

  A value that will be added to the start or end of `x`, as a numeric
  scalar.

- limit_type:

  Specify whether `limit` should be used as the `"min"` (default) or
  `"max"` value.

## Value

A numeric vector of middle values of the same length as `x`.

## Notes

An alternative function that produces the same result when the `limit`
is added at the start or end of the input `x` (e.g. `c(limit, x)` or
`c(x, limit)`) is `roll_mean(x, n = 2)` from the `RccpRoll` package.

## Examples

``` r
if (FALSE) { # \dontrun{
x <- c(7, 14, 21, 25)

roll_middle(x, 0, "min")
roll_middle(x, 30, "max")
} # }
```
