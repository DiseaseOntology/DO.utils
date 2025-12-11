# Build Hyperlinks

Build hyperlinks for Google Sheets, Excel, or HTML.
`build_hyperlinks() appends a value to the end of a base url, while `hyperlink_curie()\`
converts CURIEs to hyperlinks.

## Usage

``` r
build_hyperlink(x, url, as, ..., sep = "", text = x, preserve = "text")

hyperlink_curie(curie, as, ..., def = "obo_generic")
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

- as:

  What format to use for the hyperlink, as a string; one of "gs" (Google
  Sheet), "xlsx" (Excel), or "html".

- ...:

  *(Only for `as = "html"`)* One or more name-value pairs of html `<a>`
  [attributes](https://www.w3schools.com/tags/tag_a.asp).

- sep:

  One or more separators to use between `url` and `x`, as a character
  vector. If only one value is provided (e.g. default = ""), it will be
  recycled; otherwise the length of `sep` and `x` must match. If any
  `url` ends in the corresponding `sep`, an additional `sep` will not be
  added.

- text:

  *(OPTIONAL)* The text to display for each link, as a character vector.
  The default uses `x` as the text. If `NULL`, the full URL will serve
  as the text. If a string, the value will be used for the text of each
  hyperlink.

- preserve:

  The value to return when `url` is `NA`, as a string. One of "url" or
  "text" (default). Note that the default for `build_hyperlink()` is
  opposite the default of
  [`format_hyperlink()`](https://allenbaron.github.io/DO.utils/reference/format_hyperlink.md)
  because `text` is provided by default.

- curie:

  A character vector of CURIEs to convert to hyperlinks.

- def:

  The definition to use when checking for CURIEs. See
  [`is_curie()`](https://allenbaron.github.io/DO.utils/reference/is_curie.md)
  for details.

## See also

Functions used internally:
[`append_to_url()`](https://allenbaron.github.io/DO.utils/reference/append_to_url.md)
and
[`format_hyperlink()`](https://allenbaron.github.io/DO.utils/reference/format_hyperlink.md).

## Examples

``` r
build_hyperlink(
    x = "DiseaseOntology",
    url = "github",
    as = "html",
    text = "A hyperlink!"
)
#> [1] "<a href=\"https://github.com/DiseaseOntology\">A hyperlink!</a>"

# create CURIE links by passing local identifiers as `x` and prefixes as `url`
build_hyperlink(
    x = c("4", "D004194"),
    url = c("DOID", "MESH"),
    as = "gs",
    text = c("DOID:4", "MESH:D004194")
)
#> <googlesheets4_formula[2]>
#> [1] =HYPERLINK("http://purl.obolibrary.org/obo/DOID_4", "DOID:4")               
#> [2] =HYPERLINK("https://meshb.nlm.nih.gov/record/ui?ui=D004194", "MESH:D004194")

# provide internal URL names or direct URLs to append to
# BE SURE to use `preserve = 'url'` when text is `NA`.
build_hyperlink(
    x = c("4", "fakeID"),
    url = c("DOID", "https://madeup.url.com"),
    as = "gs",
    text = c("DOID:4", NA),
    sep = c("_", "/"),
    preserve = "url"
)
#> <googlesheets4_formula[2]>
#> [1] =HYPERLINK("http://purl.obolibrary.org/obo/DOID_4", "DOID:4")
#> [2] =HYPERLINK("https://madeup.url.com/fakeID")                  
```
