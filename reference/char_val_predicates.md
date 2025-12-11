# Character value predicates

These value predicates are designed to identify common values that
appear in character vectors.

## Usage

``` r
is_blank(x)

is_missing(x)
```

## Arguments

- x:

  vector to be tested

## Details

- `is_blank()` identifies "" or whitespace of any length

- `is_missing()` identifies NA's and blanks

## See also

Other value predicates:
[`is_invariant()`](https://allenbaron.github.io/DO.utils/reference/is_invariant.md),
[`num_val_predicates`](https://allenbaron.github.io/DO.utils/reference/num_val_predicates.md)

Other predicates:
[`all_duplicated()`](https://allenbaron.github.io/DO.utils/reference/all_duplicated.md),
[`iff_all_vals()`](https://allenbaron.github.io/DO.utils/reference/iff_all_vals.md),
[`is_curie()`](https://allenbaron.github.io/DO.utils/reference/is_curie.md),
[`is_invariant()`](https://allenbaron.github.io/DO.utils/reference/is_invariant.md),
[`is_uri()`](https://allenbaron.github.io/DO.utils/reference/is_uri.md),
[`lgl_predicates`](https://allenbaron.github.io/DO.utils/reference/lgl_predicates.md),
[`num_val_predicates`](https://allenbaron.github.io/DO.utils/reference/num_val_predicates.md),
[`obo_ID_predicates`](https://allenbaron.github.io/DO.utils/reference/obo_ID_predicates.md)
