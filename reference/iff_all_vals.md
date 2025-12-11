# Test for All Values

Returns `TRUE` if, and only if, all `values` are present in `x` and ONLY
those `values` are present in `x`.

## Usage

``` r
iff_all_vals(x, values)
```

## Arguments

- x:

  A vector.

- values:

  The values to ensure exist in `x`.

## Value

`TRUE` or `FALSE`. When `FALSE`, `missing` and/or `extra` attributes
will be included to assist in identifying non-conformity.

## See also

Other predicates:
[`all_duplicated()`](https://allenbaron.github.io/DO.utils/reference/all_duplicated.md),
[`char_val_predicates`](https://allenbaron.github.io/DO.utils/reference/char_val_predicates.md),
[`is_curie()`](https://allenbaron.github.io/DO.utils/reference/is_curie.md),
[`is_invariant()`](https://allenbaron.github.io/DO.utils/reference/is_invariant.md),
[`is_uri()`](https://allenbaron.github.io/DO.utils/reference/is_uri.md),
[`lgl_predicates`](https://allenbaron.github.io/DO.utils/reference/lgl_predicates.md),
[`num_val_predicates`](https://allenbaron.github.io/DO.utils/reference/num_val_predicates.md),
[`obo_ID_predicates`](https://allenbaron.github.io/DO.utils/reference/obo_ID_predicates.md)
