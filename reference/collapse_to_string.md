# Create a String from Inputs

Creates a single string from one or more inputs with each value
separated by `delim`.

## Usage

``` r
collapse_to_string(..., delim = "|", na.rm = FALSE, unique = FALSE)
```

## Arguments

- ...:

  One or more R objects (vector, list, data.frame, etc).

- delim:

  A delimiter to place between vector elements (default: "\|").

- na.rm:

  A logical scalar indicating whether `NA` values should be removed
  (default: `FALSE`).

- unique:

  Whether to include only unique values (across all inputs), as a
  logical.

## Examples

``` r
# collapses individual vectors
collapse_to_string(1:10)
#> [1] "1|2|3|4|5|6|7|8|9|10"

# input order is preserved
collapse_to_string(1:2, letters[1:2])
#> [1] "1|2|a|b"
collapse_to_string(data.frame(x = 1:2, y = letters[1:2]))
#> [1] "1|2|a|b"

# factor levels are captured (instead of numeric placeholders)
collapse_to_string(factor(letters[1:2]), "c")
#> [1] "a|b|c"

# unique applies across all inputs, order is determined by first appearance
collapse_to_string(c(3, 1, 2), 1:4, unique = FALSE)
#> [1] "3|1|2|1|2|3|4"
collapse_to_string(c(3, 1, 2), 1:4, unique = TRUE)
#> [1] "3|1|2|4"
```
