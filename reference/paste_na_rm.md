# Paste Omitting NA Values

Pastes elements together while omitting `NA` values, returning `NA` only
if all values are `NA`.

## Usage

``` r
paste_na_rm(..., sep = " ", collapse = NULL)
```

## Arguments

- ...:

  one or more R objects, to be converted to character vectors.

- sep:

  a character string to separate the terms. Not
  [`NA_character_`](https://rdrr.io/r/base/NA.html).

- collapse:

  an optional character string to separate the results. Not
  [`NA_character_`](https://rdrr.io/r/base/NA.html). When `collapse` is
  a string, the result is always a string
  ([`character`](https://rdrr.io/r/base/character.html) of length 1).

## Examples

``` r
paste_na_rm(
  c(letters[1:2], NA, NA),
  c(1, NA, NA, 4),
  sep = ", "
)
#> [1] "a, 1" "b"    NA     "4"   

paste_na_rm(
  c(letters[1:2], NA, NA),
  c(1, NA, NA, 4),
  sep = ", ",
  collapse = "; "
)
#> [1] "a, 1; b; 4"
```
