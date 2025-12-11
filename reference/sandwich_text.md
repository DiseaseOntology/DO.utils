# Sandwich Text Between Placeholders

Sandwiches strings between one or two placeholders.

## Usage

``` r
sandwich_text(x, placeholder, add_dup = TRUE)
```

## Arguments

- x:

  A string or character vector.

- placeholder:

  One or two placeholders to sandwich each element of `x` between. When
  two placeholders are provided, `x` will be sandwiched between them
  with the first at the start and second at the end. Otherwise, `x` will
  be sandwiched at both start and end by the same placeholder.

- add_dup:

  Whether to add placeholders even if the same character is already
  found in that position, as a boolean (default: `TRUE`).

## See also

Other general utilities:
[`length_sort()`](https://allenbaron.github.io/DO.utils/reference/length_sort.md),
[`max_paren_depth()`](https://allenbaron.github.io/DO.utils/reference/max_paren_depth.md),
[`suggest_regex()`](https://allenbaron.github.io/DO.utils/reference/suggest_regex.md)

## Examples

``` r
sandwich_text("a", placeholder = "h")
#> [1] "hah"
sandwich_text("a", placeholder = c("b", "h"))
#> [1] "bah"
sandwich_text("bah", placeholder = c("b", "h"), add_dup = TRUE)
#> [1] "bbahh"
sandwich_text("bah", placeholder = c("b", "h"), add_dup = FALSE)
#> [1] "bah"
sandwich_text("bah", placeholder = "h", add_dup = FALSE)
#> [1] "hbah"
```
