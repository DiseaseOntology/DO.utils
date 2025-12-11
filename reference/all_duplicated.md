# Identify all duplicates

Built on [`base::duplicated()`](https://rdrr.io/r/base/duplicated.html)
but, unlike
[`base::duplicated()`](https://rdrr.io/r/base/duplicated.html),
identifies all duplicates *including* the first occurrence.

## Usage

``` r
all_duplicated(x, ...)
```

## Arguments

- x:

  a (generalized, see [`is.vector`](https://rdrr.io/r/base/vector.html))
  vector, a data frame, an array or `NULL`.

- ...:

  arguments for particular methods.

## See also

Other predicates:
[`char_val_predicates`](https://allenbaron.github.io/DO.utils/reference/char_val_predicates.md),
[`iff_all_vals()`](https://allenbaron.github.io/DO.utils/reference/iff_all_vals.md),
[`is_curie()`](https://allenbaron.github.io/DO.utils/reference/is_curie.md),
[`is_invariant()`](https://allenbaron.github.io/DO.utils/reference/is_invariant.md),
[`is_uri()`](https://allenbaron.github.io/DO.utils/reference/is_uri.md),
[`lgl_predicates`](https://allenbaron.github.io/DO.utils/reference/lgl_predicates.md),
[`num_val_predicates`](https://allenbaron.github.io/DO.utils/reference/num_val_predicates.md),
[`obo_ID_predicates`](https://allenbaron.github.io/DO.utils/reference/obo_ID_predicates.md)
