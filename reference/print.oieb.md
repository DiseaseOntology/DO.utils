# Print OMIM inventory report statistics

Prints OMIM inventory report statistics.

## Usage

``` r
# S3 method for class 'oieb'
print(x, ...)
```

## Arguments

- x:

  An object of class `oieb`.

- ...:

  Arguments passed on to
  [`base::print.default`](https://rdrr.io/r/base/print.default.html)

  `digits`

  :   a non-null value for `digits` specifies the minimum number of
      significant digits to be printed in values. The default, `NULL`,
      uses
      [`getOption`](https://rdrr.io/r/base/options.html)`("digits")`.
      (For the interpretation for complex numbers see
      [`signif`](https://rdrr.io/r/base/Round.html).) Non-integer values
      will be rounded down, and only values greater than or equal to 1
      and no greater than 22 are accepted.

  `na.print`

  :   a character string which is used to indicate
      [`NA`](https://rdrr.io/r/base/NA.html) values in printed output,
      or `NULL` (see ‘Details’).

  `print.gap`

  :   a non-negative integer \\\le 1024\\, or `NULL` (meaning 1), giving
      the spacing between adjacent columns in printed vectors, matrices
      and arrays.

  `right`

  :   logical, indicating whether or not strings should be right
      aligned. The default is left alignment.

  `max`

  :   a non-null value for `max` specifies the approximate maximum
      number of entries to be printed. The default, `NULL`, uses
      [`getOption`](https://rdrr.io/r/base/options.html)`("max.print")`:
      see that help page for more details.

  `width`

  :   controls the maximum number of columns on a line used in printing
      vectors, matrices, etc. The default, `NULL`, uses
      [`getOption`](https://rdrr.io/r/base/options.html)`("width")`: see
      that help page for more details including allowed values.

  `useSource`

  :   logical, indicating whether to use source references or copies
      rather than deparsing [language
      objects](https://rdrr.io/r/base/is.language.html). The default is
      to use the original source if it is available.
