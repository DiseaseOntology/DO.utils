# Replace NULLs with specified value

Replace NULLs (in lists) with specified value. `replace_null` will
recurse into nested lists but will skip internal components that are not
lists themselves (e.g. data.frames, matrices, etc). NOTE that `replace`
will also be added to empty lists (i.e.
[`list()`](https://rdrr.io/r/base/list.html)) but not other zero-length
vectors.

## Usage

``` r
replace_null(data, replace)
```

## Arguments

- data:

  A list (or list column in a data frame).

- replace:

  A single value to use for replacement.
