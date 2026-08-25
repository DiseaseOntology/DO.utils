# Get HTML Indentation Data

Get indentation data from HTML code, including the preferred indent type
(space or tab), the base indent (the minimum indent across all lines),
and the indent increment (the minimum increment above the base indent
across all lines).

This function is useful for formatting HTML code to improve readability
and maintain consistent indentation.

## Usage

``` r
get_html_indent(html)
```

## Arguments

- html:

  HTML code as a character vector or a string with newline characters.

## Value

A list of the form `list(type = " ", base = " ", unit = " ")`, where
`type` is the preferred indent type (space or tab), `base` is the base
indent (the minimum indent across all lines), and `unit` is the indent
increment (the minimum increment above the base indent across all
lines).
