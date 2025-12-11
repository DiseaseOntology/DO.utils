# Tests for Write Access

Acts as a minimal wrapper around [file.access(mode =
2)](https://rdrr.io/r/base/file.access.html) to test whether R has
permission to write to files and/or directories.

## Usage

``` r
write_access(names)
```

## Arguments

- names:

  character vector containing file names. Tilde-expansion will be done:
  see [`path.expand`](https://rdrr.io/r/base/path.expand.html).

## Value

Logical vector of length equal to `names`. **NOTE** that this *differs*
from [`base::file.access()`](https://rdrr.io/r/base/file.access.html)
which returns an integer vector.
