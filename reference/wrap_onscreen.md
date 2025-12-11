# Wrap an On-screen Message

Wraps an on-screen message, such as those used in messages, warnings,
and errors.

## Usage

``` r
wrap_onscreen(msg, exdent = 2, ...)
```

## Arguments

- msg:

  The message to wrap, as a string.

- exdent:

  a non-negative integer specifying the indentation of subsequent lines
  in paragraphs.

- ...:

  Arguments passed on to
  [`base::strwrap`](https://rdrr.io/r/base/strwrap.html)

  `x`

  :   a character vector, or an object which can be converted to a
      character vector by
      [`as.character`](https://rdrr.io/r/base/character.html).

  `width`

  :   a positive integer giving the target column for wrapping lines in
      the output.

  `indent`

  :   a non-negative integer giving the indentation of the first line in
      a paragraph.

  `prefix,initial`

  :   a character string to be used as prefix for each line except the
      first, for which `initial` is used.

  `simplify`

  :   a logical. If `TRUE`, the result is a single character vector of
      line text; otherwise, it is a list of the same length as `x` the
      elements of which are character vectors of line text obtained from
      the corresponding element of `x`. (Hence, the result in the former
      case is obtained by unlisting that of the latter.)

## Examples

``` r
short_msg <- "This is a short message."
long_msg <- "This is a long message to demonstrate how wrap_onscreen() works. It's basically intended to be a multi-line paragraph but, as you can see if you view this variable, it is really just a very long string. We hope you enjoy!"

# no effect on short messages
message(wrap_onscreen(short_msg))
#> This is a short message.

# wrapping of longer messages, has default exdent
message(wrap_onscreen(long_msg))
#> This is a long message to demonstrate how wrap_onscreen() works. It's
#>   basically intended to be a multi-line paragraph but, as you can see
#>   if you view this variable, it is really just a very long string. We
#>   hope you enjoy!

# wrapping without exdent
message(wrap_onscreen(long_msg, exdent = 0))
#> This is a long message to demonstrate how wrap_onscreen() works. It's
#> basically intended to be a multi-line paragraph but, as you can see if
#> you view this variable, it is really just a very long string. We hope
#> you enjoy!
```
