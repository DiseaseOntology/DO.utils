# Test for Valid URIs

This is a vectorized predicate to test if character values are valid
URLs, i.e. have a scheme and at least one of: hostname or path
(according to
[RFC3986](https://www.rfc-editor.org/rfc/rfc3986#section-3)).

## Usage

``` r
is_uri(x, empty_ok = TRUE)
```

## Arguments

- x:

  A character vector.

- empty_ok:

  Whether to allow empty hosts and paths, as a boolean (default:
  `TRUE`). RFC3986 allows for empty paths & hosts, potentially making a
  scheme alone valid and, therefore, the default. However, it is often
  desirable to validate that at least one of these is NOT empty, since a
  scheme alone is rarely useful in practice.

## Value

A logical vector indicating which values of `x` are valid URIs.

## Notes

While all URIs are valid CURIEs (see `is_curie(def = "w3c")`), not all
CURIEs are valid URIs (e.g. URIs cannot start with `_`).

## See also

Other ID predicates:
[`is_curie()`](https://allenbaron.github.io/DO.utils/reference/is_curie.md),
[`obo_ID_predicates`](https://allenbaron.github.io/DO.utils/reference/obo_ID_predicates.md)

Other predicates:
[`all_duplicated()`](https://allenbaron.github.io/DO.utils/reference/all_duplicated.md),
[`char_val_predicates`](https://allenbaron.github.io/DO.utils/reference/char_val_predicates.md),
[`iff_all_vals()`](https://allenbaron.github.io/DO.utils/reference/iff_all_vals.md),
[`is_curie()`](https://allenbaron.github.io/DO.utils/reference/is_curie.md),
[`is_invariant()`](https://allenbaron.github.io/DO.utils/reference/is_invariant.md),
[`lgl_predicates`](https://allenbaron.github.io/DO.utils/reference/lgl_predicates.md),
[`num_val_predicates`](https://allenbaron.github.io/DO.utils/reference/num_val_predicates.md),
[`obo_ID_predicates`](https://allenbaron.github.io/DO.utils/reference/obo_ID_predicates.md)

## Examples

``` r
.uri <- c(
    # always TRUE
    "http://purl.obolibrary.org/obo/DOID_0001816",
    "https://google.com",
    "mailto:fake.name@blah.com",
    # TRUE, if empty_ok = FALSE
    "file://",
    "mailto:",
    # never TRUE
    "blah",
    ""
)

is_uri(.uri)
#> [1]  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE
is_uri(.uri, empty_ok = FALSE)
#> [1]  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE
```
