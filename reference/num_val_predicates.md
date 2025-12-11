# Number value predicates

These value predicates are designed to identify common values that
appear in numeric vectors.

## Usage

``` r
is_positive(x)

is_negative(x)

is_whole_number(x, tol = .Machine$double.eps)

is_scalar_whole_number(x, tol = .Machine$double.eps)
```

## Arguments

- x:

  vector to be tested

- tol:

  value specifiying precision desired (see
  [.Machine](https://rdrr.io/r/base/zMachine.html) or
  [double](https://rdrr.io/r/base/double.html) for more info)

## Details

`is_whole_number()` should generally be used when a whole number is
desired (whether integer or double) instead of
[base::is.integer](https://rdrr.io/r/base/integer.html) or the
[rlang::is_integer](https://rlang.r-lib.org/reference/type-predicates.html)
family because those test the data type no the value.

## See also

Other value predicates:
[`char_val_predicates`](https://allenbaron.github.io/DO.utils/reference/char_val_predicates.md),
[`is_invariant()`](https://allenbaron.github.io/DO.utils/reference/is_invariant.md)

Other predicates:
[`all_duplicated()`](https://allenbaron.github.io/DO.utils/reference/all_duplicated.md),
[`char_val_predicates`](https://allenbaron.github.io/DO.utils/reference/char_val_predicates.md),
[`iff_all_vals()`](https://allenbaron.github.io/DO.utils/reference/iff_all_vals.md),
[`is_curie()`](https://allenbaron.github.io/DO.utils/reference/is_curie.md),
[`is_invariant()`](https://allenbaron.github.io/DO.utils/reference/is_invariant.md),
[`is_uri()`](https://allenbaron.github.io/DO.utils/reference/is_uri.md),
[`lgl_predicates`](https://allenbaron.github.io/DO.utils/reference/lgl_predicates.md),
[`obo_ID_predicates`](https://allenbaron.github.io/DO.utils/reference/obo_ID_predicates.md)
