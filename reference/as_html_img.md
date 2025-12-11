# Construct HTML `img` Tag(s)

Vectorized construction of one or more HTML `img` tags with the
specified attributes. `src` & `alt` are required. Where they are
missing, no `img` tag will be created. Additional attributes should be
the same length as `src` or length \<= 1. Length‑1 attributes are
recycled.

## Usage

``` r
as_html_img(src, alt, ..., quote = "\"")
```

## Arguments

- src:

  The image source(s), as a character vector.

- alt:

  The alternate text, as a character vector.

- ...:

  Named character vector(s) or name-value pairs of HTML attributes.
  Attributes with values of `NULL` or `NA` will be dropped. Names should
  correspond exactly to the desired HTML attributes.

  For historical reasons, input may also be a single character vector of
  name-value pairs.

- quote:

  The quote with which to surround the attribute values. Use `""` if
  quotes are not desired (default: `"\""`, double quote).

## Value

A character vector of HTML `img` tags.
