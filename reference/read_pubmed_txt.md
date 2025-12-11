# Read in PubMed Citations (from txt file)

Reads PubMed text-format citations spanning multiple lines, usually
obtained by downloading a text file from PubMed as 'Summary (text)'.

## Usage

``` r
read_pubmed_txt(file)
```

## Arguments

- file:

  Path to .txt file; or another possible input to
  [`readLines()`](https://rdrr.io/r/base/readLines.html).

## Value

A data.frame with a record number (`n`), identifiers (`pmid`, `pmcid`,
`doi`), and the full citation (`citation`).
