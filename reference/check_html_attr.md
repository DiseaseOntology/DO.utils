# Check HTML Attribute Inputs

Checks that all HTML attribute inputs are valid, meaning (1) either
length \<= 1 or the same length as the longest attribute, (2) that all
attributes are named, and (3) that no attributes are duplicated.

## Usage

``` r
check_html_attr(..., max_length = NULL)
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
