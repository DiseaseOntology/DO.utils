# Update Table on Use Cases Page

Updates the data in the table on the "Use Cases" page of
disease-ontology.org to match the curated use cases in the
[DO_uses](https://docs.google.com/spreadsheets/d/1wG-d0wt-9YbwhQTaelxqRzbm4qnu11WDM2rv3THy5mY/?gid=1972219724#gid=1972219724)
google sheet.

## Usage

``` r
update_website_use_cases(use_cases_path, table_id = "use-cases")
```

## Arguments

- use_cases_path:

  The file path to the "Use Cases" page HTML file, as a string.

- table_id:

  The id of the table to update in the HTML file, as a string.

## Value

The page's update HTML, invisibly.
