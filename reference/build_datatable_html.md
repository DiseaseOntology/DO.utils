# Build a DataTables-Compatible HTML Table

Generates a complete `<table>` HTML string suitable for use with the
[DataTables](https://datatables.net/) JavaScript library. Each column is
described via `col_spec`, which maps display headers to data columns and
optionally to a `data-search` attribute for per-cell searching.

## Usage

``` r
build_datatable_html(
  data,
  col_spec,
  tbl_id = NULL,
  tbl_attr = list(),
  header_wrap = NULL,
  indent = 0L,
  replace_na = ""
)
```

## Arguments

- data:

  A data frame containing the table data.

- col_spec:

  A named list defining each table column. Each name becomes the `<th>`
  header text. Each element is a list with:

  - `content`: name of the column in `data` used as `<td>` content (may
    be pre-rendered HTML).

  - `search`: *(optional)* name of the column in `data` used as the
    `data-search` attribute value. `NA` values are always omitted (the
    `data-search` attribute is dropped entirely), regardless of
    `replace_na`.

  - `td_attr`: *(optional)* named list of additional static attributes
    applied to every `<td>` in the column.

- tbl_id:

  The `id` attribute for the `<table>` element, as a string.

- tbl_attr:

  A named list of additional attributes for the `<table>` element (e.g.
  `list(style = "width:100%;", class = "display")`).

- header_wrap:

  An optional length-2 character vector `c(prefix, suffix)` wrapping
  each `<th>` text. Useful for template engine syntax such as
  Babel/Jinja2, e.g. `c('{{ _("', '") }}')`.

- indent:

  Either an integer giving the starting indent level for the `<table>`
  tag (each level adds two spaces; defaults to `0L`), or an
  `html_indent` object as returned by
  [`get_html_indent()`](https://diseaseontology.github.io/DO.utils/reference/get_html_indent.md)
  to match the indentation style of an existing HTML file.

- replace_na:

  A string to replace `NA` values with, or `NULL` to leave them as-is.

## Value

A single character string containing the complete table HTML, with
newline-separated lines and consistent indentation.
