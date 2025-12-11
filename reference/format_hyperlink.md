# Format URLs as Hyperlinks

Formats URLs as hyperlinks for Google Sheets, Excel, or html.

## Usage

``` r
format_hyperlink(url, as, ..., text = NULL, preserve = "url")
```

## Arguments

- url:

  One or more URLs, as a character vector.

- as:

  What format to use for the hyperlink, as a string; one of "gs" (Google
  Sheet), "xlsx" (Excel), or "html".

- ...:

  *(Only for `as = "html"`)* One or more name-value pairs of html `<a>`
  [attributes](https://www.w3schools.com/tags/tag_a.asp).

- text:

  *(Optional)* The text to display for each link, as a character vector.
  If `NULL` (default), the URL itself will serve as the text. If only
  one value is provided, it will be recycled.

- preserve:

  The value to return when `url` is `NA`, as a string. One of "url"
  (default) or "text".

## Excel Note

Use the `openxlsx` pkg to write data with hyperlinks to Excel.

## Examples

``` r
format_hyperlink("https://www.google.com", "gs")
#> <googlesheets4_formula[1]>
#> [1] =HYPERLINK("https://www.google.com")
format_hyperlink("https://www.google.com", "xlsx")
#> [1] "https://www.google.com"
#> attr(,"class")
#> [1] "hyperlink"
format_hyperlink("https://www.google.com", "html")
#> [1] "<a href=\"https://www.google.com\">https://www.google.com</a>"

# with 'text' (argument must be named)
format_hyperlink("https://www.google.com", "gs", text = "Google")
#> <googlesheets4_formula[1]>
#> [1] =HYPERLINK("https://www.google.com", "Google")
format_hyperlink("https://www.google.com", "xlsx", text = "Google")
#> [1] "=HYPERLINK(\"https://www.google.com\", \"Google\")"
#> NULL
format_hyperlink("https://www.google.com", "html", text = "Google")
#> [1] "<a href=\"https://www.google.com\">Google</a>"

# html with <a> attributes
format_hyperlink(
    "https://www.google.com",
    "html",
    text = "Google",
    target = "_blank",
    rel = "external"
)
#> [1] "<a href=\"https://www.google.com\" target=\"_blank\" rel=\"external\">Google</a>"

# NA values in 'url' are passed through without modification by default. If
# 'text' is provided and preferred when 'url' is NA use preserve = "text".
format_hyperlink(c("https://www.google.com", NA), "gs")
#> <googlesheets4_formula[2]>
#> [1] =HYPERLINK("https://www.google.com") NA                                  
format_hyperlink(
    c("https://www.google.com", NA),
    "gs",
    text = c("Google", "placeholder")
)
#> <googlesheets4_formula[2]>
#> [1] =HYPERLINK("https://www.google.com", "Google")
#> [2] NA                                            

# 'url' is always preserved when 'text' is NA but 'url' is not
format_hyperlink(
    c("https://www.google.com/", "https://madeup.url.com/fakeID"),
    "html",
    text = c("google", NA),
    preserve = "text"
)
#> [1] "<a href=\"https://www.google.com/\">google</a>"                             
#> [2] "<a href=\"https://madeup.url.com/fakeID\">https://madeup.url.com/fakeID</a>"
```
