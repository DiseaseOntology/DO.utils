# Identify One-to-Multiple Mappings

Identifies values in `x` that map to multiple values in `y` for
specified mapping predicates.

## Usage

``` r
multimaps(
  x,
  pred,
  y,
  include_pred = c("skos:exactMatch", "skos:closeMatch", "oboInOwl:hasDbXref"),
  when_pred_NA = "error"
)
```

## Arguments

- x:

  Vector with `subject` of mappings (i.e. those being tested; the "one"
  in the "one-to-multiple" test).

- pred:

  Vector with predicate(s) of mappings. Predicate(s) should be formatted
  as CURIEs but can include multiple delimited predicates.

- y:

  Vector with `object` of mappings (i.e. those being counted; the
  "multiple" in the "one-to-multiple" test).

- include_pred:

  The predicates to include when testing for one-to-multiple mappings,
  as a character vector (default: `skos:exactMatch`, `skos:closeMatch`,
  and `oboInOwl:hasDbXref`). All other predicates are ignored.

- when_pred_NA:

  What to do when missing predicates are detected, as a string; one of
  "error" (default), "warn", or NULL (do nothing). `NA` predicates are
  *always* ignored when no mapping exists (i.e. one or both
  corresponding values of `x` or `y` is/are also `NA`).

## Value

A logical vector specifying the positions in `x` that map to multiple
values in `y`. Incomplete mappings, where values of `x`, `y`, or both
are `NA`, are ignored and return `FALSE`.
