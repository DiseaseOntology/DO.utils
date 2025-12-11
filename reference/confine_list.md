# Convert (Nested) List to/from Character Vector

These functions convert a (nested) list to or from a character vector of
the same length, preserving and restoring the list structure and values
using JSON strings. They do not perform *any* flattening or
simplification and the JSON strings are verbose.

## Usage

``` r
confine_list(.list)

release_list(.json_chr)
```

## Arguments

- .list:

  A (nested) list.

- .json_chr:

  A character vector containing the JSON representation of (nested) list
  elements below the top level.
