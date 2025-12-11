# Extract Publication Date from PubMed Citations

Extracts most complete publication date possible from Pubmed citations.

## Usage

``` r
extract_pm_date(citation)
```

## Arguments

- citation:

  character vector of PubMed citations

## Details

This function uses a step-wise approach, attempting first to extract a
full date, subsequently a year & month and, if that is not available,
just the year. This approach is designed to prevent accidental matches
to year values found in titles.

NOTE: When the day is missing, this function will return a full date
using the first day of the month. When both the month and day are
missing, this\\ function will return the first day of the year.
