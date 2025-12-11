# Match Length-2+ Vector Arguments

Matches arguments with several inputs allowed against a set of choices.
Similar to [`base::match.arg()`](https://rdrr.io/r/base/match.arg.html)
with `several.ok = TRUE` *EXCEPT* `match_arg_several()` will signal an
error for unmatching values in `arg` instead of silently dropping them.

## Usage

``` r
match_arg_several(arg, choices)
```

## Arguments

- arg:

  Function argument, as a character vector, or `NULL`.

- choices:

  Candidate values, as a character vector.
