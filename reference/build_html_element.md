# Build HTML Element(s)

Vectorized construction of one or more HTML elements including the
specified tag(s), attribute(s) and content. Length‑1 inputs will be
recycled to the length of the longest input. All inputs with length \>
1, must be of the same length.

## Usage

``` r
build_html_element(
  tag,
  ...,
  content = NULL,
  close_empty = TRUE,
  quote = "\"",
  include = "optional"
)
```

## Arguments

- tag:

  The tag(s) to include, as a character vector. Length‑1 character
  vectors will be recycled to the length of the

- ...:

  Named character vector(s) or name-value pairs of HTML attributes.
  Attributes with values of `NULL` or `NA` will be dropped. Names should
  correspond exactly to the desired HTML attributes.

  For historical reasons, input may also be a single character vector of
  name-value pairs.

- content:

  The content to include within the tag(s), as a character vector. The
  default, `NULL`, means no content is intended. For tags that are
  required to be empty, content will be ignored. Length‑1 content will
  be recycled.

- close_empty:

  Whether to close empty tags (e.g. `<img>`) with a space and slash
  (default: `TRUE` for compatibility with XHTML). For a reference, see
  https://stackoverflow.com/questions/1946426/html-5-is-it-br-br-or-br.

- quote:

  The quote with which to surround the attribute values. Use `""` if
  quotes are not desired (default: `"\""`, double quote).

- include:

  The type(s) of tag(s) to include from the output; one of "optional"
  (default, including both optional & required tags) or "required"
  (including *ONLY* required tags).

  Take extra care when using `include = "required"`. There are many
  cases where a tag considered "optional" is actually required.
  `build_html_element()` only handles two possible scenarios: (1) both
  tags are included when `content` has a value, and (2) start tags are
  included when there are corresponding attributes. To understand tag
  requirements, review this [table of HTML
  tags](https://www.w3.org/TR/html401/index/elements.html) and the
  [HTML5
  syntax](https://www.w3.org/TR/2011/WD-html5-20110405/syntax.html).

## Value

A character vector of HTML elements, including start tags along with
attributes, content, and/or end tags as appropriate.
