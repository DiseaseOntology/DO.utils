# Write Data to a Google Sheet

Specialized methods for writing data created by `DO.utils` to a
specified Google Sheet.

## Usage

``` r
write_gs(data, ss, sheet = NULL, hyperlink_curie = NULL, ...)

# S3 method for class 'omim_inventory'
write_gs(
  data,
  ss,
  sheet = "omim_inventory-%Y%m%d",
  hyperlink_curie = c("omim", "doid"),
  ...
)

# S3 method for class 'data.frame'
write_gs(data, ss, sheet = "data-%Y%m%d", hyperlink_curie = NULL, ...)
```

## Arguments

- data:

  A data.frame, possibly with a defined method.

- ss:

  Something that identifies a Google Sheet:

  - its file id as a string or
    [`drive_id`](https://googledrive.tidyverse.org/reference/drive_id.html)

  - a URL from which we can recover the id

  - a one-row
    [`dribble`](https://googledrive.tidyverse.org/reference/dribble.html),
    which is how googledrive represents Drive files

  - an instance of `googlesheets4_spreadsheet`, which is what
    [`gs4_get()`](https://googlesheets4.tidyverse.org/reference/gs4_get.html)
    returns

  Processed through
  [`as_sheets_id()`](https://googlesheets4.tidyverse.org/reference/sheets_id.html).

- sheet:

  The name to use for the sheet to write into (i.e. the tab name in a
  worksheet). If a date format recognized by
  [`format.Date()`](https://rdrr.io/r/base/as.Date.html) is included in
  the string, today's date will be added in the specified format and
  location.

  For each method, the default is a method-specific term appended with a
  date format lacking separators (e.g. 20250101 for 2025-01-01).

  ***WARNING:*** If a sheet with the same name exists in the Google
  Sheet file it will be overwritten.

- hyperlink_curie:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  The columns with CURIEs to convert to hyperlinks when written in
  Google Sheets.

- ...:

  Arguments passed on to methods.

## Value

The input `ss`, as an instance of
[googlesheets4::sheets_id](https://googlesheets4.tidyverse.org/reference/sheets_id.html).

## Deprecated arguments

`sheet_nm` and `datestamp` have been deprecated in favor of `sheet`
which supports more flexible naming, including names without dates.
