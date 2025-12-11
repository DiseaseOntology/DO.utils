# Make "Contributors" HTML

Makes the "Contributor" `<li>` elements for disease-ontology.org. Can be
used for any ontology given the appropriate input.

## Usage

``` r
make_contributor_html(contrib_df)
```

## Arguments

- contrib_df:

  A data.frame with information about contributors, including the
  required columns: 'name', 'team_member', 'github', and 'orcid'.
  'github' and 'orcid' columns can have data missing but at least one
  should be present for each contributor.

## Examples

``` r
if (FALSE) { # interactive()
trans_contributors <- googlesheets4::read_sheet(
    ss = "1kD7rgOWO2uVUwKYoKFSLBEpv1WZFf-GDhEusAq_H5sM",
    sheet = "TRANS",
    col_types = "c"
) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), readr::parse_guess))
trans_contributors

make_contributors_html(trans_contributors)
}
```
