# Sort Vectors by Priority

Sort vectors, prioritizing according to levels specified. Uses
[`base::factor()`](https://rdrr.io/r/base/factor.html) internally with
unspecified arguments disabled.

## Usage

``` r
priority_sort(x, levels, exclude = NA)
```

## Arguments

- x:

  a vector of data, usually taking a small number of distinct values.

- levels:

  an optional vector of the unique values (as character strings) that
  `x` might have taken. The default is the unique set of values taken by
  [`as.character`](https://rdrr.io/r/base/character.html)`(x)`, sorted
  into increasing order *of `x`*. Note that this set can be specified as
  smaller than `sort(unique(x))`.

- exclude:

  a vector of values to be excluded when forming the set of levels. This
  may be factor with the same level set as `x` or should be a
  `character`.
