# Make HTML for DO Use Case Tables

Makes the row and cell html code for the various sections/tables of the
disease-ontology.org "Use Cases" page from the DO team's "DO_uses"
google sheet. This function explicitly avoids including the html code
for defining the table itself to provide for flexibility. The "html"
output in the files specified must be manually copied and pasted into
the disease-ontology.org "Use Cases" file in the appropriate
section/table.

## Usage

``` r
make_use_case_html(out_dir = "graphics/website", group = "all")
```

## Arguments

- out_dir:

  The path to the directory where output should be saved, as a string.

- group:

  The group(s) to generate html for, as a character vector. One or more
  of: "all" (default) or specific values in the `type` column of the
  [DO_uses](https://docs.google.com/spreadsheets/d/1wG-d0wt-9YbwhQTaelxqRzbm4qnu11WDM2rv3THy5mY/?gid=1972219724#gid=1972219724)
  `DO_website_user_list` sheet.

## Value

One "html" file in `out_dir` for each `group` named as
"DO_use_case-{group}.html" and the "User" data from the Google Sheet
invisibly.
