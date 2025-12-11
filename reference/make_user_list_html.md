# Make HTML for DO User List (DEPRECATED)

Makes the row and cell html code for the "Users of the Disease Ontology"
section of the collaborators page on disease-ontology.org from the DO
team's "Uses" google sheet. This function explicitly avoids including
the html code for defining the table itself to provide for flexibility.

## Usage

``` r
make_user_list_html(file)
```

## Arguments

- file:

  The file path where the output should be saved, as a string.

## Deprecation Notice

The information this was formatting for disease-ontology.org was moved
from the "Collaborators" page to the new "Use Cases" page in mid-2022
and was split from one section into three, making this function
obsolete. Use
[`make_use_case_html()`](https://allenbaron.github.io/DO.utils/reference/make_use_case_html.md)
instead.
