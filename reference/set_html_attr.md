# Set HTML Attributes

Sets attributes for one or more HTML tags in a vectorized manner.

## Usage

``` r
set_html_attr(..., max_length = NULL, quote = "\"")
```

## Arguments

- ...:

  Named character vector(s) or name-value pairs of HTML attributes.
  Attributes with values of `NULL` or `NA` will be dropped. Names should
  correspond exactly to the desired HTML attributes.

  For historical reasons, input may also be a single character vector of
  name-value pairs.

- max_length:

  The maximum expected length of attributes, as an integer. If `NULL`
  (default), the maximum length will be the length of the longest
  attribute.

- quote:

  The quote with which to surround the attribute values. Use `""` if
  quotes are not desired (default: `"\""`, double quote).

## Value

A character vector of HTML attribute strings including necessary quotes
and with a leading space, e.g.
`' src="img path" alt="img alt text here"'`.

## Notes

- Each input to `...` and `quote` must be a length‑1 vector, a vector of
  the same length as the longest input, or a vector of length equal to
  `max_length`. Length‑1 vectors are recycled.

- No checking is done to confirm names correspond to true HTML
  attributes. Beware of spelling errors!
