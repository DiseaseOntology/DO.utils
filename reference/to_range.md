# Convert Vectors to Range String

Converts vectors to a string of ranges. All vector types are accepted
and the original values will appear in the final output range, but where
input is not a numeric vector of [whole
numbers](https://allenbaron.github.io/DO.utils/reference/is_whole_number()),
a function to convert the values to integers must be provided (`int_fn`)
for the purpose of identifying the range(s).

## Usage

``` r
to_range(
  x,
  int_fn = NULL,
  ...,
  sep = c(",", "-"),
  start_rm = NULL,
  end_rm = NULL
)
```

## Arguments

- x:

  A numeric vector of [whole
  numbers](https://allenbaron.github.io/DO.utils/reference/is_whole_number()).

- int_fn:

  A function (or tidyverse-style formula) to convert `x` into an integer
  vector, used *ONLY for creating ranges*; the original value will
  appear in the range except where modified by `start_rm` and/or
  `end_rm`. `int_fun` is required when `x` is not a numeric vector.

- ...:

  Arguments passed on to `int_fn`.

- sep:

  The separators to use between ranges (default: ',') and within a range
  (default: '-'), as a length-2 character vector.

- start_rm:

  A regular expression to remove from `x` values at the *beginning* of a
  range.

- end_rm:

  A regular expression to remove from `x` values at the *end* of a
  range.

## Value

The range(s) formatted as a string or `NA` if the input is an empty
vector.

## Notes

- `NA` values are always dropped.

- `to_range()` was inspired by answers at
  https://stackoverflow.com/q/16911773/6938922, most heavily by speendo
  (CC-BY-SA 3.0, accessed 2022-07-01). This is the fastest approach with
  few inputs but is significantly slower than other answers for large
  inputs. The internal approach will likely be modified in the future,
  but arguments and output will remain the same.

## Examples

``` r
x <- c(1:2, 8:6, 4, -1:-2, 20:37, 4, 40, 43, 45)
to_range(x)
#> [1] "-2,-1,1,2,4,6-8,20-37,40,43,45"


# `NA` values are dropped
y <- c(1:4, NA, 5, 7:10)
to_range(x)
#> [1] "-2,-1,1,2,4,6-8,20-37,40,43,45"


# Use `int_fn` when `x` is not a numeric vector (tidyverse-style formulas
#    accepted)
x_char <- as.character(x)
to_range(x, int_fn = as.integer)
#> [1] "-2,-1,1,2,4,6-8,20-37,40,43,45"
to_range(x, int_fn = ~ as.integer(.x))
#> [1] "-2,-1,1,2,4,6-8,20-37,40,43,45"


# `int_fn` allows non-numeric ranges to be created
txt <- paste0(x, "txt")
to_int <- function(x, y) as.integer(stringr::str_remove(x, "txt"))
to_range(txt, to_int, y = "txt")
#> [1] "-2txt,-1txt,1txt,2txt,4txt,6txt-8txt,20txt-37txt,40txt,43txt,45txt"


# text can be selectively removed from the values at the beginning of ranges
#     (`start_rm`) or end of ranges (`end_rm`)
to_range(txt, to_int, start_rm = "txt")
#> [1] "-2txt,-1txt,1txt,2txt,4txt,6-8txt,20-37txt,40txt,43txt,45txt"
to_range(txt, to_int, end_rm = "txt")
#> [1] "-2txt,-1txt,1txt,2txt,4txt,6txt-8,20txt-37,40txt,43txt,45txt"
```
