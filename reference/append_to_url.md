# Append to URL

Append one or more value(s) to a corresponding URL.

## Usage

``` r
append_to_url(x, url, sep = "")
```

## Arguments

- x:

  Value(s) to append, as a character vector.

- url:

  One or more URLs or URL names recognized by this package, as a
  character vector. If only one value is provided, it will be recycled;
  otherwise the length of `url` and `x` must match. See
  [`get_url()`](https://allenbaron.github.io/DO.utils/reference/get_url.md)
  for recognized base URL names.

- sep:

  One or more separators to use between `url` and `x`, as a character
  vector. If only one value is provided (e.g. default = ""), it will be
  recycled; otherwise the length of `sep` and `x` must match. If any
  `url` ends in the corresponding `sep`, an additional `sep` will not be
  added.

## Note

No URL validation is performed.

## Examples

``` r
append_to_url("blah", "http://fake.url.com/")
#> [1] "http://fake.url.com/blah"

# separator can be specified and will not be duplicated
append_to_url("blah", "http://fake.url.com/", sep = "?q=")
#> [1] "http://fake.url.com/?q=blah"
append_to_url("blah", "http://fake.url.com/", sep = "/")
#> [1] "http://fake.url.com/blah"

# vectorized w/recycling
append_to_url(c("blah", "ugh"), "http://fake.url.com", sep = "/")
#> [1] "http://fake.url.com/blah" "http://fake.url.com/ugh" 
append_to_url(
    c("blah", "ugh"),
    c("http://fake.url.com", "https://madeup.url.com/"),
    sep = "/"
)
#> [1] "http://fake.url.com/blah"   "https://madeup.url.com/ugh"
append_to_url(
    c("blah", "ugh"),
    c("http://fake.url.com", "https://madeup.url.com/"),
    sep = c("/", "?q=")
)
#> [1] "http://fake.url.com/blah"      "https://madeup.url.com/?q=ugh"

# missing values in `x` or `url` are preserved
append_to_url(
    c(NA, "uhhh"),
    c("http://fake.url.com", "https://this.is.it.com/"),
    sep = "/"
)
#> [1] NA                            "https://this.is.it.com/uhhh"
append_to_url(
    c(NA, "uhhh"),
    c("http://fake.url.com", NA_character_),
    c("=", NA)
)
#> [1] NA NA

# `sep` must not be missing for non-missing values of `x` and `url`
try(
    append_to_url(
        c(NA, "uhhh"),
        c("http://fake.url.com", "https://this.is.it.com/"),
        sep = c("/", NA)
    )
)
#> Error in purrr::pmap_chr(list(url, x, sep, ignore_sep), function(.u, .x,  : 
#>   ℹ In index: 2.
#> Caused by error in `paste()`:
#> ! invalid separator

# invalid URIs return NA
append_to_url("ha", "not_a_url")
#> [1] NA
append_to_url("ha", "unknown.scheme:") # technically a valid URI
#> [1] "unknown.scheme:ha"
```
