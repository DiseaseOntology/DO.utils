# Download Alliance .tsv.gz File

Downloads a URL-specified .tsv.gz file from the Alliance of Genome
Resources. Files can be found at
<https://www.alliancegenome.org/downloads>. Right-click on the "tsv"
link of a desired file and select "Copy Link" to get the file URL.

## Usage

``` r
download_alliance_tsv(dest_dir, url = NULL, ...)
```

## Arguments

- dest_dir:

  path to directory where file will be saved

- url:

  URL to Alliance file; if not provided, will be requested at console

- ...:

  Additional arguments passed on to
  [`utils::download.file()`](https://rdrr.io/r/utils/download.file.html).

## Value

Path to saved file.

## Details

A date stamp indicating download date is added to the base file name.

## Recommendation

Although it's possbile to directly read a file from the URL, downloading
it promotes reproducibility and ensures future access if needed.

## See also

Other Alliance functions:
[`count_alliance_records()`](https://allenbaron.github.io/DO.utils/reference/count_alliance_records.md),
[`read_alliance()`](https://allenbaron.github.io/DO.utils/reference/read_alliance.md),
[`save_alliance_counts()`](https://allenbaron.github.io/DO.utils/reference/save_alliance_counts.md)
