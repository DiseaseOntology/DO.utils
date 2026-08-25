# Update 'Registry of Contributor' Tables on Web Pages

Updates data in the 'Registry of Contributor' tables of
disease-ontology.org to match the curated contributor data in the
[DO_contributors](https://docs.google.com/spreadsheets/d/1kD7rgOWO2uVUwKYoKFSLBEpv1WZFf-GDhEusAq_H5sM/)
google sheet.

## Usage

``` r
update_website_contributors(contrib_path, table_id)
```

## Arguments

- contrib_path:

  The file path to the "Contributors" page HTML file, as a string.

- table_id:

  The id of the table to update in the HTML file, as a string.

## Value

The page's updated HTML, invisibly.
