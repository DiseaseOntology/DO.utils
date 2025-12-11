# Convert Vectors to Strings

Concatenate values in a vector into a single string. \* `vctr_to_string`
performs simple concatenation. \* `unique_to_string` reduces the vector
to unique values prior to concatenation.

## Usage

``` r
vctr_to_string(
  x,
  delim = "|",
  na.rm = FALSE,
  sort = FALSE,
  decreasing = FALSE,
  ...
)

unique_to_string(
  x,
  delim = "|",
  na.rm = FALSE,
  sort = FALSE,
  decreasing = FALSE,
  ...
)
```

## Arguments

- x:

  A vector.

- delim:

  A delimiter to place between vector elements (default: "\|").

- na.rm:

  A logical scalar indicating whether `NA` values should be removed
  (default: `FALSE`).

- sort:

  A logical scalar indicating whether values should be sorted (default:
  `FALSE`).

- decreasing:

  logical. Should the sort be increasing or decreasing? For the
  `"radix"` method, this can be a vector of length equal to the number
  of arguments in `...`. For the other methods, it must be length one.
  Not available for partial sorting.

- ...:

  arguments to be passed to or from methods or (for the default methods
  and objects without a class) to `sort.int`.

## See also

[`unique_if_invariant()`](https://allenbaron.github.io/DO.utils/reference/unique_if_invariant.md)
for an alternative, *conditional* `unique` vector-to-string conversion
method
