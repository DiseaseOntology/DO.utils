# Write Data to a Google Sheet

Specialized methods for writing data created by `DO.utils` to a
specified Google Sheet.

## Usage

``` r
write_gs(data, ss, hyperlink_curie, sheet_nm, datestamp, ...)

# S3 method for class 'omim_inventory'
write_gs(
  data,
  ss,
  hyperlink_curie = c("omim", "doid"),
  sheet_nm = "omim_inventory",
  datestamp = "%Y%m%d",
  ...
)

# S3 method for class 'data.frame'
write_gs(
  data,
  ss,
  hyperlink_curie = NULL,
  sheet_nm = "data",
  datestamp = "%Y%m%d",
  ...
)
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

- hyperlink_curie:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  The columns with CURIEs to convert to hyperlinks when written in
  Google Sheets.

- sheet_nm:

  The name of the sheet to write to, as a string.

- datestamp:

  `NULL` to use the default sheet name for a given method, or a format
  recognized by [`format.Date()`](https://rdrr.io/r/base/as.Date.html)
  to add today's date as a stamp suffix, separated by '-', to the
  default sheet name.

- ...:

  Arguments passed on to methods.

## Value

The data as written to the Google Sheet, invisibly.
