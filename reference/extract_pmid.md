# Extract PubMed ID

`extract_pmid` is a generic function that extracts PubMed IDs.

## Usage

``` r
extract_pmid(x, ...)

# S3 method for class 'pm_search'
extract_pmid(x, ...)

# S3 method for class 'pmc_search'
extract_pmid(x, ...)

# S3 method for class 'data.frame'
extract_pmid(x, ...)
```

## Arguments

- x:

  An object.

- ...:

  Arguments passed on to methods.

## Value

A character vector of PubMed IDs, except for the
`extract_pmid.elink_list` method which returns a list of PubMed ID
character vectors.

## See also

Other extract_pmid documentation:
[`extract_pmid.elink()`](https://allenbaron.github.io/DO.utils/reference/extract_pmid.elink.md),
[`extract_pmid.elink_list()`](https://allenbaron.github.io/DO.utils/reference/extract_pmid.elink_list.md)
