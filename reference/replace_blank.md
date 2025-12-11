# Replace Blanks with Specified Value

Replace blanks with specified value.

## Usage

``` r
replace_blank(data, replace = NA_character_, ...)
```

## Arguments

- data:

  A data object.

- replace:

  A string to use for replacement.

- ...:

  Additional arguments passed on to methods. Not currently used.

## Value

- If `data` is a vector, `replace_blank()` returns a vector of the same
  class as `data` (only blanks in character vectors are modified).

- If `data` is a list, `replace_blank()` will recurse into the list (as
  necessary) and replace all blank values in character/list elements but
  will skip other internal components (e.g. data.frames, matrices, etc).
