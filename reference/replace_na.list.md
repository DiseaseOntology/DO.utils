# \[DEPRECATED\] Replace NAs with specified value

Replace all NAs in lists with specified value. This method of
`replace_na` will recurse into nested lists but will skip internal
components that are not lists or simple vectors themselves (e.g.
data.frames, matrices, etc). **NOTE \[REQUIRES `tidyr <= 1.1.4`\]:**
Coercion will occur where vectors in the list are of different types
than `replace`, with either the vector or `replace` being
coercedaccording to type order: logical \< integer \< numeric \< complex
\< character \< list.

## Usage

``` r
# S3 method for class 'list'
replace_na(data, replace, ...)
```

## Arguments

- data:

  A data frame or vector.

- replace:

  If `data` is a data frame, `replace` takes a named list of values,
  with one value for each column that has missing values to be replaced.
  Each value in `replace` will be cast to the type of the column in
  `data` that it being used as a replacement in.

  If `data` is a vector, `replace` takes a single value. This single
  value replaces all of the missing values in the vector. `replace` will
  be cast to the type of `data`.

- ...:

  Additional arguments for methods. Currently unused.

## DEPRECATION NOTE

The "unsafe" type conversion allowed by `replace_na.list()` will not
work with `tidyr v1.2.0`, which uses "safe" type conversion via
[`vctrs::vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.html).
`replace_na.list()` will work with `tidyr <= 1.1.4`. If
`replace_na.list()` is not frequently needed, it will be removed in
future updates.
