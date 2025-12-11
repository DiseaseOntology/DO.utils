# Test if an Object is Invariant

Test if an object is invariant (*i.e.* all values are equal, within a
given tolerance for numeric vectors).

## Usage

``` r
is_invariant(x, ...)

# Default S3 method
is_invariant(x, na.rm = FALSE, ...)

# S3 method for class 'numeric'
is_invariant(x, na.rm = FALSE, tol = sqrt(.Machine$double.eps), ...)

# S3 method for class 'list'
is_invariant(x, incl_nm = TRUE, ...)

# S3 method for class 'data.frame'
is_invariant(x, ...)
```

## Arguments

- x:

  object to be tested

- ...:

  unused; for extensibility

- na.rm:

  logical indicating whether to exclude NA values

- tol:

  double, tolerance to use (for numeric vectors)

- incl_nm:

  Whether top-level names should be included in determining if a list is
  invariant (default: `TRUE`).

## See also

Other value predicates:
[`char_val_predicates`](https://allenbaron.github.io/DO.utils/reference/char_val_predicates.md),
[`num_val_predicates`](https://allenbaron.github.io/DO.utils/reference/num_val_predicates.md)

Other predicates:
[`all_duplicated()`](https://allenbaron.github.io/DO.utils/reference/all_duplicated.md),
[`char_val_predicates`](https://allenbaron.github.io/DO.utils/reference/char_val_predicates.md),
[`iff_all_vals()`](https://allenbaron.github.io/DO.utils/reference/iff_all_vals.md),
[`is_curie()`](https://allenbaron.github.io/DO.utils/reference/is_curie.md),
[`is_uri()`](https://allenbaron.github.io/DO.utils/reference/is_uri.md),
[`lgl_predicates`](https://allenbaron.github.io/DO.utils/reference/lgl_predicates.md),
[`num_val_predicates`](https://allenbaron.github.io/DO.utils/reference/num_val_predicates.md),
[`obo_ID_predicates`](https://allenbaron.github.io/DO.utils/reference/obo_ID_predicates.md)
