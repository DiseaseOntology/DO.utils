# Guess Data Delimiter

Identifies the delimiter of data as being the greater of "," or "\t"
present in all or most lines.

## Usage

``` r
guess_delim(x, strict = TRUE)
```

## Arguments

- x:

  String or character data for which to guess delimiter.

- strict:

  Whether delimiter should be found in all lines (default: `TRUE`), or
  just a majority of lines. If `FALSE`, an attempt is made to guess the
  delimiter for data that may not be entirely delimited.
